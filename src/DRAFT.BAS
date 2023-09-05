'----------------------------------------
'            DRAFT Subroutine
'----------------------------------------
'This subroutine presents a menu to the
'user with choices for drafting players.
Sub DRAFT ()

    _Title "3-in-1 Football - Team Draft"

    ReDim menuChoices$(1 To 3)

    Do

        Call TITLE

        menuChoices$(1) = "DRAFT A NEW TEAM         "
        menuChoices$(2) = "DRAFT TO AN EXISTING TEAM"
        menuChoices$(3) = "EXIT TO MAIN MENU        "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 11, 50, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:

                    Call TITLE

                    For XY = 2 To 23:
                        Locate XY, 1: Print Tab(79);
                    Next

                    Locate 1, 1: Color 15, 0

                    Call NewTeamParameters(YN$, team$, teamNbr)
                    Call DraftChoices(YN$, team$)
                    Call DoTheDraft(1, doneDraft, YN$, team$, teamNbr)

                Case 2:

                    Call ChooseDraftTeam(YN$, NTMS, teamNbr, team$)

                    If teamNbr > 0 Then
                        Call DraftExistingTeam(YN$, teamNbr)
                        Call ExistingTeamParameters(YN$, team$, teamNbr)
                        Call DraftChoices(YN$, team$)
                        Call DoTheDraft(0, doneDraft, YN$, team$, teamNbr)
                    End If

            End Select

            If doneDraft = 1 Then Selection% = 3

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'      NewTeamParameters Subroutine
'----------------------------------------
'This subroutine prompts the user for the
'minimum required parameters to create a
'new team that players will be drafted to.
Sub NewTeamParameters (YN$, teamName$, teamIdx)

    Shared ydsCompAdj
    Shared teamAttendance&

    Shared teamNick$

    Shared leagueRatings%()
    Shared teamRatings%()

    Locate 3: Input " 1. TEAM # (BLANK TO AUTO-ASSIGN) "; teamIdx
    Locate 4: Input " 2. DISK ID  "; YN$
    Locate 5: Input " 3. TEAM NAME (IE, 94 ARKANSAS) "; teamName$
    Locate 6: Input " 4. TEAM NICKNAME (IE, PANTHERS) "; teamNick$
    Locate 7: Input " 5. DEF RUSH ADJ "; leagueRatings%(0)
    Locate 8: Input " 6. DEF COMP % ADJ "; leagueRatings%(1)
    Locate 9: Input " 7. DEF INT % ADJ "; leagueRatings%(2)
    Locate 10: Input " 8. FUM % (AVG=4) "; leagueRatings%(3)
    Locate 11: Input " 9. DEF FUM % (AVG=0) "; leagueRatings%(4)
    Locate 12: Input "10. OFF SACKED % (AVG=8) "; leagueRatings%(5)
    Locate 13: Input "11. DEF SACK ADJ "; leagueRatings%(6)
    Locate 14: Input "12. DEF YD/COMP ADJ "; ydsCompAdj
    Locate 15: Input "13. TM PENALTY ADJ "; teamRatings%(0)
    Locate 16: Input "14. OFF RUN/PASS % "; leagueRatings%(7)
    Locate 17: Input "15. TM STADIUM (0-OUTDOOR 1-DOME) "; teamRatings%(3)
    Locate 18: Input "16. STADIUM NAME "; S$
    Locate 19: Input "17. AVERAGE ATTENDANCE "; teamAttendance&
    Locate 20: Input "18. POWER RATING "; teamRatings%(4)
    Locate 21: Input "19. TEXT COLOR (0-15) "; teamRatings%(2)
    Locate 22: Input "20. BACKGROUND COLOR (0-7) "; teamRatings%(1)

    If teamRatings%(1) >= 0 And teamRatings%(2) >= 0 Then
        Locate 21, 37
        Print "TEAM APPEARANCE: ";
        Locate 22, 37
        Color teamRatings%(2), teamRatings%(1): Print RTrim$(A$) + " " + RTrim$(mascot$)
        Color 15, 0
    End If

    Do

        Do
            Locate 24
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 6

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3: Input " 1. TEAM # (BLANK TO AUTO-ASSIGN) "; teamIdx

                Case 2
                    Locate 4: Input " 2. DISK ID  "; YN$

                Case 3
                    Locate 5: Input " 3. TEAM NAME (IE, 94 ARKANSAS) "; teamName$

                Case 4
                    Locate 6: Input " 4. TEAM NICKNAME (IE, PANTHERS) "; teamNick$

                Case 5
                    Locate 7: Input " 5. DEF RUSH ADJ "; leagueRatings%(0)

                Case 6
                    Locate 8: Input " 6. DEF COMP % ADJ "; leagueRatings%(1)

                Case 7
                    Locate 9: Input " 7. DEF INT % ADJ "; leagueRatings%(2)

                Case 8
                    Locate 10: Input " 8. FUM % (AVG=4) "; leagueRatings%(3)

                Case 9
                    Locate 11: Input " 9. DEF FUM % (AVG=0) "; leagueRatings%(4)

                Case 10
                    Locate 12: Input "10. OFF SACKED % (AVG=8) "; leagueRatings%(5)

                Case 11
                    Locate 13: Input "11. DEF SACK ADJ "; leagueRatings%(6)

                Case 12
                    Locate 14: Input "12. DEF YD/COMP ADJ "; ydsCompAdj

                Case 13
                    Locate 15: Input "13. TM PENALTY ADJ "; teamRatings%(0)

                Case 14
                    Locate 16: Input "14. OFF RUN/PASS % "; leagueRatings%(7)

                Case 15
                    Locate 17: Input "15. TM STADIUM (0-OUTDOOR 1-DOME) "; teamRatings%(3)

                Case 16
                    Locate 18: Input "16. STADIUM NAME "; S$

                Case 17
                    Locate 19: Input "17. AVERAGE ATTENDANCE "; teamAttendance&

                Case 18
                    Locate 20: Input "18. POWER RATING "; teamRatings%(4)

                Case 19
                    Locate 21: Input "19. TEXT COLOR (0-15) "; teamRatings%(2)

                    If teamRatings%(1) >= 0 And teamRatings%(2) >= 0 Then
                        Locate 21, 37
                        Print "TEAM APPEARANCE: ";
                        Locate 22, 37
                        Color teamRatings%(2), teamRatings%(1): Print RTrim$(A$) + " " + RTrim$(mascot$)
                        Color 15, 0
                    End If

                Case 20
                    Locate 22: Input "20. BACKGROUND COLOR (0-7) "; teamRatings%(1)

                    If teamRatings%(1) >= 0 And teamRatings%(2) >= 0 Then
                        Locate 21, 37
                        Print "TEAM APPEARANCE: ";
                        Locate 22, 37
                        Color teamRatings%(2), teamRatings%(1): Print RTrim$(A$) + " " + RTrim$(mascot$)
                        Color 15, 0
                    End If

            End Select

        End If

    Loop Until B9 = 0

End Sub


'----------------------------------------
'      DraftExistingTeam Subroutine
'----------------------------------------
'This subroutine assigns the team data from
'the team we are drafting to a new array,
'then faciliates the selection of a team
'that we will be drafting from. This will,
'in turn, assign those values to the extra
'array that is available.
Sub DraftExistingTeam (YN$, teamIdx)

    Shared stadium$

    Shared leagueRatings%(), teamRatings%()

    Shared rushAtt(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared wrRecept(), wrRecAvg()
    Shared passAtt(), compPct(), qbIntPct()
    Shared krRet(), krRetAvg(), prRet(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGPct(), pkPAT(), pkPATPct()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

    Shared rbAtt_DRAFT(), rbAvg_DRAFT(), rbRec_DRAFT(), rbRecAvg_DRAFT(), wrRec_DRAFT(), wrAvg_DRAFT(), qbPA_DRAFT(), qbPCPct_DRAFT(), qbIntPct_DRAFT(), krNumRet_DRAFT()
    Shared krRetAvg_DRAFT(), prRet_DRAFT(), prRetAvg_DRAFT(), puntAvg_DRAFT(), FGA_DRAFT(), FGPct_DRAFT(), leagRat_DRAFT(), PAT_DRAFT(), PATPct_DRAFT()
    Shared qbArmRat_DRAFT(), numInt_DRAFT(), numSack_DRAFT(), tmRat_DRAFT()

    Shared rbNam_DRAFT$(), wrNam_DRAFT$(), qbNam_DRAFT$(), krNam_DRAFT$(), prNam_DRAFT$(), pntNam_DRAFT$(), pkNam_DRAFT$()
    Shared intNam_DRAFT$(), sackNam_DRAFT$()

    Shared teamAttendance&

    For I = 0 To 7:
        leagueRatings%(I) = leagRat_DRAFT(I)
    Next I

    For I = 0 To 4:
        teamRatings%(I) = tmRat_DRAFT(I)
    Next I

    For I = 0 To 9
        rbName$(I) = rbNam_DRAFT$(I)
        rushAtt(I) = rbAtt_DRAFT(I)
        rushAvg(I) = rbAvg_DRAFT(I)
        rbRecepts(I) = rbRec_DRAFT(I)
        rbRecAvg(I) = rbRecAvg_DRAFT(I)
    Next I

    For I = 0 To 5
        wrName$(I) = wrNam_DRAFT$(I)
        wrRecept(I) = wrRec_DRAFT(I)
        wrRecAvg(I) = wrAvg_DRAFT(I)
    Next I

    For I = 0 To 3
        qbName$(I) = qbNam_DRAFT$(I)
        passAtt(I) = qbPA_DRAFT(I)
        compPct(I) = qbPCPct_DRAFT(I)
        qbIntPct(I) = qbIntPct_DRAFT(I)
        armRating(I) = qbArmRat_DRAFT(I)
    Next I

    For I = 0 To 2
        krName$(I) = krNam_DRAFT$(I)
        krRet(I) = krNumRet_DRAFT(I)
        krRetAvg(I) = krRetAvg_DRAFT(I)
    Next I

    For I = 0 To 2
        prName$(I) = prNam_DRAFT$(I)
        prRet(I) = prRet_DRAFT(I)
        prRetAvg(I) = prRetAvg_DRAFT(I)
    Next I

    punterName$(0) = pntNam_DRAFT$(0)
    puntAvg(0) = puntAvg_DRAFT(0)

    pkName$(0) = pkNam_DRAFT$(0)
    pkFGA(0) = FGA_DRAFT(0)
    pkFGPct(0) = FGPct_DRAFT(0)
    pkPAT(0) = PAT_DRAFT(0)
    pkPATPct(0) = PATPct_DRAFT(0)
    pkName$(1) = pkNam_DRAFT$(1)
    pkFGA(1) = FGA_DRAFT(1)
    pkFGPct(1) = FGPct_DRAFT(1)
    pkPAT(1) = PAT_DRAFT(1)
    pkPATPct(1) = PATPct_DRAFT(1)

    For I = 0 To 14
        sackName$(I) = sackNam_DRAFT$(I)
        numSacks(I) = numSack_DRAFT(I)
    Next I

    For I = 0 To 9
        intName$(I) = intNam_DRAFT$(I)
        numInts(I) = numInt_DRAFT(I)
    Next I

    If Not _FileExists(diskPaths$(0) + DATA_FILE_NAME$ + "." + YN$) Then

        stadium$ = ""
        teamAttendance& = 0

        MsgText$ = "WARNING!!! NO DATA FILE WAS FOUND FOR THIS TEAM. PRESS ANY KEY TO CONTINUE"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(0) + DATA_FILE_NAME$ + "." + YN$ For Random As #1 Len = DATA_SIZE_BYTES

        Field #1, 15 As X$, 30 As Q$(1), 4 As Q$(3), 1 As Q$(4)

        Get #1, teamIdx

        stadium$ = RTrim$(Q$(1))
        teamAttendance& = CVL(Q$(3))
        Z$ = Q$(4)

        Close #1

        If Z$ <> "x" Then
            stadium$ = ""
            teamAttendance& = 0
        End If

    End If

End Sub


'----------------------------------------
'    ExistingTeamParameters Subroutine
'----------------------------------------
'This subroutine simply prints the ratings
'and values from the existing team so the
'end user can review them before proceeding.
Sub ExistingTeamParameters (YN$, teamName$, teamIdx)

    Shared ydsCompAdj
    Shared teamAttendance&

    Shared teamNick$, stadium$

    Shared leagueRatings%()
    Shared teamRatings%()

    Call TITLE

    For XY = 2 To 23:
        Locate XY, 1: Print Tab(79);
    Next

    Locate 1, 1: Color 15, 0
    Locate 3: Print " 1. ENTER DISK ID FOR THIS TEAM "; YN$
    Locate 4: Print " 2. TEAM # "; teamIdx
    Locate 5: Print " 3. TEAM NAME "; teamName$
    Locate 6: Print " 4. TEAM NICKNAME "; teamNick$
    Locate 7: Print " 5. DEF RUSH ADJ "; leagueRatings%(0)
    Locate 8: Print " 6. DEF COMP % ADJ "; leagueRatings%(1)
    Locate 9: Print " 7. DEF INT % ADJ "; leagueRatings%(2)
    Locate 10: Print " 8. FUM % "; leagueRatings%(3)
    Locate 11: Print " 9. DEF FUM % "; leagueRatings%(4)
    Locate 12: Print "10. OFF SACKED % "; leagueRatings%(5)
    Locate 13: Print "11. DEF SACK ADJ "; leagueRatings%(6)
    Locate 14: Print "12. DEF YD/COMP ADJ "; ydsCompAdj
    Locate 15: Print "13. TM PENALTY ADJ "; teamRatings%(0)
    Locate 16: Print "14. OFF RUN/PASS % "; leagueRatings%(7)
    Locate 17: Print "15. TM STADIUM (0-OUTDOOR 1-DOME) "; teamRatings%(3)
    Locate 18: Print "16. POWER RATING "; teamRatings%(4)
    Locate 19: Print "17. TM BACKGROUND COLOR (0-7) "; teamRatings%(1)
    Locate 20: Print "18. TM TEXT COLOR (0-15) "; teamRatings%(2)
    Locate 21: Print "19. STADIUM NAME "; stadium$
    Locate 22: Print "20. AVERAGE ATTENDANCE "; teamAttendance&

End Sub


'----------------------------------------
'       DraftChoices Subroutine
'----------------------------------------
'This subroutine provides an interface the
'user can operate to choose their draft
'selections. For each position chosen, a
'source team will need selected.
Sub DraftChoices (YN$, teamName$)

    Shared leagueRatings%(), teamRatings%()

    Shared rushAtt(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared wrRecept(), wrRecAvg()
    Shared passAtt(), compPct(), qbIntPct()
    Shared krRet(), krRetAvg(), prRet(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGPct(), pkPAT(), pkPATPct()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

    Shared rbAtt_DRAFT(), rbAvg_DRAFT(), rbRec_DRAFT(), rbRecAvg_DRAFT(), wrRec_DRAFT(), wrAvg_DRAFT(), qbPA_DRAFT(), qbPCPct_DRAFT(), qbIntPct_DRAFT(), krNumRet_DRAFT()
    Shared krRetAvg_DRAFT(), prRet_DRAFT(), prRetAvg_DRAFT(), puntAvg_DRAFT(), FGA_DRAFT(), FGPct_DRAFT(), PAT_DRAFT(), PATPct_DRAFT()
    Shared qbArmRat_DRAFT(), tmRat_DRAFT()

    Shared rbNam_DRAFT$(), wrNam_DRAFT$(), qbNam_DRAFT$(), krNam_DRAFT$(), prNam_DRAFT$(), pntNam_DRAFT$(), pkNam_DRAFT$()

    Call TITLE

    Do

        For XY = 2 To 23:
            Locate XY, 1: Print Tab(79);
        Next

        Locate 5
        Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "R";: Color 3: Print "UNNING BACKS"
        Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "W";: Color 3: Print "IDE RECEIVERS"
        Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "Q";: Color 3: Print "UARTERBACKS"
        Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "K";: Color 3: Print "ICKOFF RETURN"
        Locate , 5: Color 3: Print "DRAFT ";: Color 15: Print "P";: Color 3: Print "UNT RETURN"
        Locate , 5: Color 3: Print "DRAFT P";: Color 15: Print "U";: Color 3: Print "NTERS"
        Locate , 5: Color 3: Print "DRAFT PLACE K";: Color 15: Print "I";: Color 3: Print "CKERS"
        Locate , 5: Color 15: Print "E";: Color 3: Print "ND AND SAVE DRAFT"

        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "R":

                Call ChooseDraftTeam(draftYear$, NTMS, draftIdx, draftTeam$)

                If draftIdx <> 0 Then

                    Do
                        Call TITLE

                        For XY = 2 To 23:
                            Locate XY, 1: Print Tab(79);
                        Next

                        Locate 2, 33
                        Color 15, 0: Print "RUNNING BACKS"
                        Locate 3, 1

                        Color teamRatings%(2), teamRatings%(1): Print teamName$;
                        Color 7, 0
                        Print Tab(17); "RUN AVG CX% RC AVG CX%";

                        Print Tab(41);

                        Color tmRat_DRAFT(2), tmRat_DRAFT(1): Print draftTeam$;
                        Color 7, 0
                        Print Tab(57); "RUN AVG CX% RC AVG CX%"

                        Color 15, 0

                        For I = 0 To 9: Locate I + 4, 1
                            Print I; " "; rbName$(I); Tab(19);
                            Print Using "##.#  ###   ##  ###"; rushAvg(I) * .3; rushAtt(I); rbRecAvg(I) * 3; rbRecepts(I)
                            Locate I + 4, 41
                            Print I; " "; rbNam_DRAFT$(I); Tab(59);
                            Print Using "##.#  ###   ##  ###"; rbAvg_DRAFT(I) * .3; rbAtt_DRAFT(I); rbRecAvg_DRAFT(I) * 3; rbRec_DRAFT(I)
                        Next I

                        Do
                            Locate 16,
                            Print "SLOT TO DRAFT # (0-9, 10=NONE) ";: Input P
                        Loop Until P >= 0 And P <= 10

                        If P <> 10 Then
                            Do
                                Locate 16, 45
                                Print "PLAYER # TO DRAFT ";: Input P1
                            Loop Until P1 >= 0 And P1 <= 9

                            rbName$(P) = rbNam_DRAFT$(P1)
                            rushAtt(P) = rbAtt_DRAFT(P1)
                            rushAvg(P) = rbAvg_DRAFT(P1)
                            rbRecepts(P) = rbRec_DRAFT(P1)
                            rbRecAvg(P) = rbRecAvg_DRAFT(P1)

                        End If

                    Loop Until P = 10

                End If

            Case "W":

                Call ChooseDraftTeam(draftYear$, NTMS, draftIdx, draftTeam$)

                If draftIdx <> 0 Then

                    Do
                        Call TITLE

                        For XY = 2 To 23:
                            Locate XY, 1: Print Tab(79);
                        Next

                        Locate 2, 33
                        Color 15, 0: Print "WIDE RECEIVERS"
                        Locate 3, 1

                        Color teamRatings%(2), teamRatings%(1): Print teamName$;
                        Color 7, 0
                        Print Tab(17); "REC AVG  CX% ";

                        Print Tab(41);

                        Color tmRat_DRAFT(2), tmRat_DRAFT(1): Print draftTeam$;
                        Color 7, 0
                        Print Tab(57); "REC AVG  CX%"

                        Color 15, 0

                        For I = 0 To 5:
                            Locate I + 4, 1: Print I; " "; wrName$(I);
                            Print Tab(17); Using "  ##    ###"; wrRecAvg(I) * 3; wrRecept(I)
                            Locate I + 4, 41
                            Print I; " "; wrNam_DRAFT$(I);
                            Print Tab(57); Using "  ##    ###"; wrAvg_DRAFT(I) * 3; wrRec_DRAFT(I)
                        Next I

                        Do
                            Locate 16, 5
                            Print "SLOT TO DRAFT # (0-5, 6=NONE) ";: Input P
                        Loop Until P >= 0 And P <= 6

                        If P <> 6 Then
                            Do
                                Locate 16, 45: Print "PLAYER # TO DRAFT ";: Input P1
                            Loop Until P1 >= 0 And P1 <= 5

                            wrName$(P) = wrNam_DRAFT$(P1)
                            wrRecept(P) = wrRec_DRAFT(P1)
                            wrRecAvg(P) = wrAvg_DRAFT(P1)

                        End If

                    Loop Until P = 6

                End If

            Case "Q":

                Call ChooseDraftTeam(draftYear$, NTMS, draftIdx, draftTeam$)

                If draftIdx <> 0 Then

                    Do
                        Call TITLE

                        For XY = 2 To 23:
                            Locate XY, 1: Print Tab(79);
                        Next

                        Locate 2, 34
                        Color 15, 0: Print "QUARTERBACKS"
                        Locate 3, 1

                        Color teamRatings%(2), teamRatings%(1): Print teamName$;
                        Color 7, 0: Print Tab(17); " C%  I%   ARM  CX% ";

                        Print Tab(41);

                        Color tmRat_DRAFT(2), tmRat_DRAFT(1): Print draftTeam$;

                        Color 7, 0: Print Tab(57); " C%  I%   ARM  CX%"

                        Color 15, 0

                        For I = 0 To 3: Locate I + 4, 1
                            Print I; " "; qbName$(I);
                            Print Tab(17); Using "###  ##  ###  ###"; compPct(I); qbIntPct(I); armRating(I); passAtt(I)
                            Locate I + 4, 41
                            Print I; " "; qbNam_DRAFT$(I);
                            Print Tab(57); Using "###  ##  ###  ###"; qbPCPct_DRAFT(I); qbIntPct_DRAFT(I); qbArmRat_DRAFT(I); qbPA_DRAFT(I)
                        Next I

                        Do
                            Locate 16, 5
                            Print "SLOT TO DRAFT # (0-3, 4=NONE) ";: Input P
                        Loop Until P >= 0 And P <= 4

                        If P <> 4 Then

                            Do
                                Locate 16, 45
                                Print "PLAYER # TO DRAFT ";: Input P1
                            Loop Until P1 >= 0 And P1 <= 3

                            qbName$(P) = qbNam_DRAFT$(P1)
                            passAtt(P) = qbPA_DRAFT(P1)
                            compPct(P) = qbPCPct_DRAFT(P1)
                            qbIntPct(P) = qbIntPct_DRAFT(P1)
                            armRating(P) = qbArmRat_DRAFT(P1)

                        End If

                    Loop Until P = 4

                End If

            Case "K":

                Call ChooseDraftTeam(draftYear$, NTMS, draftIdx, draftTeam$)

                If draftIdx <> 0 Then

                    Do
                        Call TITLE

                        For XY = 2 To 23:
                            Locate XY, 1: Print Tab(79);
                        Next

                        Locate 2, 34
                        Color 15, 0: Print "KICKOFF RETURNERS"
                        Locate 3, 1

                        Color teamRatings%(2), teamRatings%(1): Print teamName$;
                        Color 7, 0
                        Print Tab(17); "RET AVG  CX%";

                        Print Tab(41);

                        Color tmRat_DRAFT(2), tmRat_DRAFT(1): Print draftTeam$;
                        Color 7, 0
                        Print Tab(57); "RET AVG  CX%"

                        Color 15, 0

                        For I = 0 To 2: Locate I + 4, 1
                            Print I; " "; krName$(I);
                            Print Tab(17); Using "  ##     ###"; krRetAvg(I) * 3; krRet(I)
                            Locate I + 4, 41
                            Print I; " "; krNam_DRAFT$(I);
                            Print Tab(57); Using "  ##     ###"; krRetAvg_DRAFT(I) * 3; krNumRet_DRAFT(I)
                        Next I

                        Do
                            Locate 16, 5
                            Print "SLOT TO DRAFT (0-2, 3=NONE) ";: Input P
                        Loop Until P >= 0 And P <= 3

                        If P <> 3 Then

                            Do
                                Locate 16, 45
                                Print "PLAYER # TO DRAFT ";: Input P1
                            Loop Until P1 >= 0 And P1 <= 2

                            krName$(P) = krNam_DRAFT$(P1)
                            krRet(P) = krNumRet_DRAFT(P1)
                            krRetAvg(P) = krRetAvg_DRAFT(P1)

                        End If

                    Loop Until P = 3

                End If

            Case "P":

                Call ChooseDraftTeam(draftYear$, NTMS, draftIdx, draftTeam$)

                If draftIdx <> 0 Then

                    Do
                        Call TITLE

                        For XY = 2 To 23:
                            Locate XY, 1: Print Tab(79);
                        Next

                        Locate 2, 34
                        Color 15, 0: Print "PUNT RETURNERS"
                        Locate 3, 1

                        Color teamRatings%(2), teamRatings%(1): Print teamName$;
                        Color 7, 0
                        Print Tab(17); "RET AVG  CX%";

                        Print Tab(41);

                        Color tmRat_DRAFT(2), tmRat_DRAFT(1): Print draftTeam$;
                        Color 7, 0
                        Print Tab(57); "RET AVG  CX%"

                        Color 15, 0

                        For I = 0 To 2
                            Locate I + 4, 1
                            Print I; " "; prName$(I);
                            Print Tab(17); Using "  ##    ###"; prRetAvg(I) * 3; prRet(I)
                            Locate I + 4, 41
                            Print I; " "; prNam_DRAFT$(I);
                            Print Tab(57); Using "  ##    ###"; prRetAvg_DRAFT(I) * 3; prRet_DRAFT(I)
                        Next I

                        Do
                            Locate 16, 5
                            Print "SLOT TO DRAFT (0-2, 3=NONE) ";: Input P
                        Loop Until P >= 0 And P <= 3

                        If P <> 3 Then
                            Do
                                Locate 16, 45
                                Print "PLAYER # TO DRAFT ";: Input P1
                            Loop Until P1 >= 0 And P1 <= 2

                            prName$(P) = prNam_DRAFT$(P1)
                            prRet(P) = prRet_DRAFT(P1)
                            prRetAvg(P) = prRetAvg_DRAFT(P1)

                        End If

                    Loop Until P = 3

                End If

            Case "U":

                Call ChooseDraftTeam(draftYear$, NTMS, draftIdx, draftTeam$)

                If draftIdx <> 0 Then

                    Do
                        Call TITLE

                        For XY = 2 To 23:
                            Locate XY, 1: Print Tab(79);
                        Next

                        Locate 2, 37
                        Color 15, 0: Print "PUNTER"
                        Locate 3, 1

                        Color teamRatings%(2), teamRatings%(1): Print teamName$;
                        Color 7, 0
                        Print Tab(17); "PUNT AVG";

                        Print Tab(41);

                        Color tmRat_DRAFT(2), tmRat_DRAFT(1): Print draftTeam$;
                        Color 7, 0
                        Print Tab(57); "PUNT AVG"

                        Color 15, 0

                        Locate 4, 1
                        Print 0; " "; punterName$(0);
                        Print Tab(17); Using "   ##"; puntAvg(0) * 2 + 28

                        Locate 4, 41
                        Print 0; " "; pntNam_DRAFT$(0);
                        Print Tab(57); Using "   ##"; puntAvg_DRAFT(0) * 2 + 28

                        Do
                            Locate 16, 5
                            Print "PLAYER # (0, 1=NONE) ";: Input P
                        Loop Until P >= 0 And P <= 1

                        If P <> 1 Then
                            Locate 20, 1: Print "HIT ANY KEY TO CONTINUE"
                            I$ = GetKeyPress$

                            punterName$(0) = pntNam_DRAFT$(0)
                            puntAvg(0) = puntAvg_DRAFT(0)
                        End If

                    Loop Until P = 1

                End If

            Case "I":

                Call ChooseDraftTeam(draftYear$, NTMS, draftIdx, draftTeam$)

                If draftIdx <> 0 Then

                    Do
                        Call TITLE

                        For XY = 2 To 23:
                            Locate XY, 1: Print Tab(79);
                        Next

                        Locate 2, 34
                        Color 15, 0: Print "PLACE KICKERS"
                        Locate 3, 1

                        Color teamRatings%(2), teamRatings%(1): Print teamName$;
                        Color 7, 0
                        Print Tab(17); "FG%  CX   EP%  CX";

                        Print Tab(41);

                        Color tmRat_DRAFT(2), tmRat_DRAFT(1): Print draftTeam$;
                        Color 7, 0
                        Print Tab(57); "FG%  CX   EP%  CX"

                        Color 15, 0

                        For I = 0 To 1:
                            Locate I + 4, 1
                            Print I; " "; pkName$(I);
                            Print Tab(17); Using "### ###   ### ###"; pkFGA(I) * 5 + 25; pkFGPct(I); pkPAT(I); pkPATPct(I)
                            Locate I + 4, 41
                            Print I; " "; pkNam_DRAFT$(I);
                            Print Tab(57); Using "### ###   ### ###"; FGA_DRAFT(I) * 5 + 25; FGPct_DRAFT(I); PAT_DRAFT(I); PATPct_DRAFT(I)
                        Next I

                        Do
                            Locate 16, 5
                            Print "PLAYER # (0-1, 2=NONE) ";: Input P
                        Loop Until P >= 0 And P <= 2

                        If P <> 2 Then
                            Do
                                Locate 16, 45
                                Print "PLAYER # TO DRAFT ";: Input P1
                            Loop Until P1 >= 0 And P1 <= 1

                            pkName$(P) = pkNam_DRAFT$(P1)
                            pkFGA(P) = FGA_DRAFT(P1)
                            pkFGPct(P) = FGPct_DRAFT(P1)
                            pkPAT(P) = PAT_DRAFT(P1)
                            pkPATPct(P) = PATPct_DRAFT(P1)

                        End If

                    Loop Until P = 2

                End If

            Case "E":
                Call TITLE

                For XY = 2 To 23:
                    Locate XY, 1: Print Tab(79);
                Next

        End Select

    Loop Until UCase$(I$) = "E"

End Sub


'----------------------------------------
'       DoTheDraft Subroutine
'----------------------------------------
'This subroutine calculates all the values
'and stats that need to be swapped and then
'performs the trading / swapping.
Sub DoTheDraft (draftNew, doneDrafting, YN$, teamName$, teamIdx)

    Shared ydsCompAdj

    Shared teamAttendance&

    Shared teamNick$

    Shared leagueRatings%(), teamRatings%()

    Shared rushAtt(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared wrRecept(), wrRecAvg()
    Shared passAtt(), compPct(), qbIntPct()
    Shared krRet(), krRetAvg(), prRet(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGPct(), pkPAT(), pkPATPct()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

    S0 = 0
    S1 = 0
    S2 = 0
    S3 = 0

    For I = 0 To 9:
        S0 = S0 + rushAtt(I)
        S2 = S2 + rbRecepts(I)
    Next

    S4 = 100 / S0

    For I = 0 To 9:
        If rushAtt(I) <> 0 Then rushAtt(I) = Int(S4 * rushAtt(I) + .5) + S1: S1 = rushAtt(I)
        If rushAtt(I) > 100 Then rushAtt(I) = 100
    Next I

    For I = 0 To 5:
        S2 = S2 + wrRecept(I)
    Next

    S5 = 100 / S2

    For I = 0 To 9:
        If rbRecepts(I) <> 0 Then rbRecepts(I) = Int(S5 * rbRecepts(I) + .5) + S3: S3 = rbRecepts(I)
        If rbRecepts(I) > 100 Then rbRecepts(I) = 100
    Next I

    For I = 0 To 5:
        If wrRecept(I) <> 0 Then wrRecept(I) = Int(S5 * wrRecept(I) + .5) + S3: S3 = wrRecept(I)
        If wrRecept(I) > 100 Then wrRecept(I) = 100
    Next I

    S6 = 0
    S7 = 0

    For I = 0 To 3:
        S6 = S6 + passAtt(I)
    Next

    S8 = 100 / S6

    For I = 0 To 3:
        If passAtt(I) <> 0 Then passAtt(I) = Int(S8 * passAtt(I) + .5) + S7: S7 = passAtt(I)
        If passAtt(I) > 100 Then passAtt(I) = 100
    Next

    G0 = 0
    G1 = 0
    G3 = 0
    G4 = 0

    For I = 0 To 2:
        G0 = G0 + krRet(I)
        G3 = G3 + prRet(I)
    Next

    G2 = 100 / G0
    G5 = 100 / G3

    For I = 0 To 2:
        If krRet(I) <> 0 Then krRet(I) = Int(G2 * krRet(I) + .5) + G1: G1 = krRet(I)
        If krRet(I) > 100 Then krRet(I) = 100
    Next

    For I = 0 To 2:
        If prRet(I) <> 0 Then prRet(I) = Int(G5 * prRet(I) + .5) + G4: G4 = prRet(I)
        If prRet(I) > 100 Then prRet(I) = 100
    Next

    G0 = 0
    G1 = 0
    G3 = 0
    G4 = 0

    For I = 0 To 1:
        G0 = G0 + pkFGPct(I)
        G3 = G3 + pkPATPct(I)
    Next

    G2 = 100 / G0
    G5 = 100 / G3

    For I = 0 To 1:
        If pkFGPct(I) <> 0 Then pkFGPct(I) = Int(G2 * pkFGPct(I) + .5) + G1: G1 = pkFGPct(I)
        If pkFGPct(I) > 100 Then pkFGPct(I) = 100
    Next

    For I = 0 To 1:
        If pkPATPct(I) <> 0 Then pkPATPct(I) = Int(G5 * pkPATPct(I) + .5) + G4: G4 = pkPATPct(I)
        If pkPATPct(I) > 100 Then pkPATPct(I) = 100
    Next

    For I = 9 To 0 Step -1:
        If rushAtt(I) <> 0 Then rushAtt(I) = 100: Exit For
    Next

    For I = 5 To 0 Step -1:
        If wrRecept(I) <> 0 Then wrRecept(I) = 100: Exit For
    Next

    For I = 3 To 0 Step -1:
        If passAtt(I) <> 0 Then passAtt(I) = 100: Exit For
    Next

    For I = 2 To 0 Step -1:
        If krRet(I) <> 0 Then krRet(I) = 100: Exit For
    Next

    For I = 2 To 0 Step -1:

        If prRet(I) <> 0 Then prRet(I) = 100: Exit For
    Next

    For I = 1 To 0 Step -1:
        If pkFGPct(I) <> 0 Then pkFGPct(I) = 100: Exit For
    Next

    For I = 1 To 0 Step -1:
        If pkPATPct(I) <> 0 Then pkPATPct(I) = 100: Exit For
    Next

    X = Int((ydsCompAdj / .3) + .5)
    If X < 0 Then X = 100 + X
    ydsCompAdj = X

    Call SaveTeamDraft(draftNew, YN$, teamName$, teamIdx)

    Cls

    JJ = 1

    Locate 6, 8: Color 15, 1

    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

    For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

    Locate 7

    U$ = "DRAFT ANOTHER TEAM? (Y/N)"

    Do
        Locate 7, (80 - Len(U$)) / 2
        Print U$
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "N" Then
        doneDrafting = 1
    End If

End Sub


'----------------------------------------
'       SaveTeamDraft Subroutine
'----------------------------------------
'This subroutine updates the source team
'with the newly assigned player / stat
'values after the draft.
'
'draftNew
'  0 = Existing Team
'  1 = New Team
Sub SaveTeamDraft (draftNew, yearNumber$, teamName$, teamIdx)

    Shared teamAttendance&

    Shared ydsCompAdj

    Shared teamNick$, stadium$

    Shared leagueRatings%(), teamRatings%()

    Shared rushAtt(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared wrRecept(), wrRecAvg()
    Shared passAtt(), compPct(), qbIntPct()
    Shared krRet(), krRetAvg(), prRet(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGPct(), pkPAT(), pkPATPct()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

    teamYear$ = RTrim$(yearNumber$)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    Close #1

    If draftNew = 1 Then teamIdx = (fileLength& / TEAM_SIZE_BYTES) + 1

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

    LSet Q$(1) = teamName$
    LSet Q$(2) = teamNick$

    For I = 0 To 7: LSet Q$(I + 3) = MKI$(leagueRatings%(I)): Next
    For I = 0 To 4: LSet Q$(I + 11) = MKI$(teamRatings%(I)): Next I

    For I = 0 To 9
        LSet Q$(I * 5 + 16) = rbName$(I)
        LSet Q$(I * 5 + 17) = MKI$(rushAtt(I))
        LSet Q$(I * 5 + 18) = MKI$(rushAvg(I))
        LSet Q$(I * 5 + 19) = MKI$(rbRecepts(I))
        LSet Q$(I * 5 + 20) = MKI$(rbRecAvg(I))
    Next I

    For I = 0 To 5
        LSet Q$(I * 3 + 66) = wrName$(I)
        LSet Q$(I * 3 + 67) = MKI$(wrRecept(I))
        LSet Q$(I * 3 + 68) = MKI$(wrRecAvg(I))
    Next I

    For I = 0 To 3
        LSet Q$(I * 5 + 84) = qbName$(I)
        LSet Q$(I * 5 + 85) = MKI$(passAtt(I))
        LSet Q$(I * 5 + 86) = MKI$(compPct(I))
        LSet Q$(I * 5 + 87) = MKI$(qbIntPct(I))
        LSet Q$(I * 5 + 88) = MKI$(armRating(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 104) = krName$(I)
        LSet Q$(I * 3 + 105) = MKI$(krRet(I))
        LSet Q$(I * 3 + 106) = MKI$(krRetAvg(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 113) = prName$(I)
        LSet Q$(I * 3 + 114) = MKI$(prRet(I))
        LSet Q$(I * 3 + 115) = MKI$(prRetAvg(I))
    Next I

    test$ = punterName$(0)
    LSet Q$(122) = punterName$(0)

    LSet Q$(123) = MKI$(puntAvg(0))
    LSet Q$(124) = pkName$(0)
    LSet Q$(125) = MKI$(pkFGA(0))
    LSet Q$(126) = MKI$(pkFGPct(0))
    LSet Q$(127) = MKI$(pkPAT(0))
    LSet Q$(128) = MKI$(pkPATPct(0))
    LSet Q$(129) = pkName$(1)
    LSet Q$(130) = MKI$(pkFGA(1))
    LSet Q$(131) = MKI$(pkFGPct(1))
    LSet Q$(132) = MKI$(pkPAT(1))
    LSet Q$(133) = MKI$(pkPATPct(1))
    LSet Q$(134) = MKI$(ydsCompAdj)

    For I = 0 To 14
        LSet Q$(I * 2 + 135) = sackName$(I)
        LSet Q$(I * 2 + 136) = MKI$(numSacks(I))
    Next I

    For I = 0 To 9
        LSet Q$(I * 2 + 165) = intName$(I)
        LSet Q$(I * 2 + 166) = MKI$(numInts(I))
    Next I

    Put #2, teamIdx

    Close #2

    Open diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$ For Random As #3 Len = DATA_SIZE_BYTES

    Field #3, 15 As Q$(1), 30 As Q$(2), 4 As Q$(3), 1 As Q$(4)

    LSet Q$(1) = teamName$
    LSet Q$(2) = stadium$
    LSet Q$(3) = MKL$(teamAttendance&)
    LSet Q$(4) = "x"

    Put #3, teamIdx

    Close #3

End Sub


'----------------------------------------
'       ChooseDraftTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for
'choosing the team to draft players from
Sub ChooseDraftTeam (yearNumber$, numberTeams, teamIdx, teamName$)

    Shared tmRat_DRAFT()

    Shared previousYear$, teamNick$

    Do

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 4)

        Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: "
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If Not (NY$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(NY$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAM FILES WERE FOUND MATCHING THIS ID. PRESS ANY KEY TO RETURN TO MAIN MENU"
                        BoxWidth = 20
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                        Exit Sub

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex%(teamIdx) = teamIdx
                        Next teamIdx

                        Close #1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectDraftTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%)

                If ExitCode% <> 27 Then

                    Do
                        Locate 2, 33
                        Color tmRat_DRAFT(2), tmRat_DRAFT(1): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N"

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectDraftTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for
'selecting the team to draft players from
Sub SelectDraftTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%)

    If numberTeams > 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColor% = 15: FileBGColor% = 4
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Rows% = 20
        Columns% = 4
        Spacing% = 6
        Row% = 4
        Column% = 1

        Color , 4
        Cls
        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate 25, 1
        Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(SELECTION%)

            Locate 2, 33: Color 3
            Print "LOADING.."

            Call ReadSourceTeam(yearNumber$, teamName$, selectedIdx)

        Else

            BO% = 1

        End If

    End If

End Sub

'----------------------------------------
'     ReadSourceTeam Subroutine
'----------------------------------------
'This subroutine reads team data into
'arrays for the selected team
Sub ReadSourceTeam (teamYear$, targetTeam$, teamIdx)

    Dim C2!(15), C3!(15)

    Shared ydsCompAdj

    Shared teamNick$

    Shared FGA_DRAFT(), FGPct_DRAFT(), leagRat_DRAFT(), PAT_DRAFT(), PATPct_DRAFT()
    Shared krNumRet_DRAFT(), krRetAvg_DRAFT()
    Shared numInt_DRAFT(), numSack_DRAFT()
    Shared prRet_DRAFT(), prRetAvg_DRAFT(), puntAvg_DRAFT()
    Shared qbArmRat_DRAFT(), qbPA_DRAFT(), qbPCPct_DRAFT(), qbIntPct_DRAFT()
    Shared rbAtt_DRAFT(), rbAvg_DRAFT(), rbRec_DRAFT(), rbRecAvg_DRAFT()
    Shared tmRat_DRAFT(), wrRec_DRAFT(), wrAvg_DRAFT()

    Shared rbNam_DRAFT$(), wrNam_DRAFT$(), qbNam_DRAFT$(), krNam_DRAFT$(), prNam_DRAFT$(), pntNam_DRAFT$(), pkNam_DRAFT$()
    Shared intNam_DRAFT$(), sackNam_DRAFT$()

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    Field #1, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)

    Field #1, 56 As Q$(0), 12 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 2 As Q$(20), 12 As Q$(21), 2 As Q$(22), 2 As Q$(23), 2 As Q$(24), 2 As Q$(25), 200 As Q$, 200 As Q$(200), 238 As Q$(201), 200 As Q$

    Field #1, 96 As Q$(0), 12 As Q$(26), 2 As Q$(27), 2 As Q$(28), 2 As Q$(29), 2 As Q$(30), 12 As Q$(31), 2 As Q$(32), 2 As Q$(33), 2 As Q$(34), 2 As Q$(35), 200 As Q$(199), 200 As Q$(200), 198 As Q$(201), 200 As Q$

    Field #1, 136 As Q$(0), 12 As Q$(36), 2 As Q$(37), 2 As Q$(38), 2 As Q$(39), 2 As Q$(40), 12 As Q$(41), 2 As Q$(42), 2 As Q$(43), 2 As Q$(44), 2 As Q$(45), 200 As Q$(199), 200 As Q$(200), 158 As Q$(201), 200 As Q$

    Field #1, 176 As Q$(0), 12 As Q$(46), 2 As Q$(47), 2 As Q$(48), 2 As Q$(49), 2 As Q$(50), 12 As Q$(51), 2 As Q$(52), 2 As Q$(53), 2 As Q$(54), 2 As Q$(55), 200 As Q$(199), 200 As Q$(200), 118 As Q$(201), 200 As Q$

    Field #1, 216 As Q$(0), 12 As Q$(56), 2 As Q$(57), 2 As Q$(58), 2 As Q$(59), 2 As Q$(60), 12 As Q$(61), 2 As Q$(62), 2 As Q$(63), 2 As Q$(64), 2 As Q$(65), 200 As Q$(199), 200 As Q$(200), 78 As Q$(201), 200 As Q$

    Field #1, 200 As Q$(0), 56 As Q$, 12 As Q$(66), 2 As Q$(67), 2 As Q$(68), 12 As Q$(69), 2 As Q$(70), 2 As Q$(71), 12 As Q$(72), 2 As Q$(73), 2 As Q$(74), 200 As Q$(99), 200 As Q$(200), 200 As Q$, 30 As Q$

    Field #1, 200 As Q$(0), 100 As Q$, 4 As Q$(201), 12 As Q$(75), 2 As Q$(76), 2 As Q$(77), 12 As Q$(78), 2 As Q$(79), 2 As Q$(80), 12 As Q$(81), 2 As Q$(82), 2 As Q$(83), 200 As Q$(199), 182 As Q$(200), 200 As Q$

    Field #1, 200 As Q$(0), 152 As Q$(201), 12 As Q$(84), 2 As Q$(85), 2 As Q$(86), 2 As Q$(87), 2 As Q$(88), 12 As Q$(89), 2 As Q$(90), 2 As Q$(91), 2 As Q$(92), 2 As Q$(93), 200 As Q$(199), 200 As Q$(200), 142 As Q$(202)

    Field #1, 200 As Q$(0), 192 As Q$(201), 12 As Q$(94), 2 As Q$(95), 2 As Q$(96), 2 As Q$(97), 2 As Q$(98), 12 As Q$(99), 2 As Q$(100), 2 As Q$(101), 2 As Q$(102), 2 As Q$(103), 200 As Q$(199), 200 As Q$(200), 102 As Q$(202)

    Field #1, 200 As Q$(0), 200 As Q$(199), 32 As Q$(200), 12 As Q$(104), 2 As Q$(105), 2 As Q$(106), 12 As Q$(107), 2 As Q$(108), 2 As Q$(109), 12 As Q$(110), 2 As Q$(111), 2 As Q$(112), 200 As Q$(201), 200 As Q$(202), 54 As Q$(203)

    Field #1, 200 As Q$(0), 200 As Q$(199), 80 As Q$(200), 12 As Q$(113), 2 As Q$(114), 2 As Q$(115), 12 As Q$(116), 2 As Q$(117), 2 As Q$(118), 12 As Q$(119), 2 As Q$(120), 2 As Q$(121), 200 As Q$(201), 200 As Q$(202), 6 As Q$(203)

    Field #1, 200 As Q$, 200 As Q$, 128 As Q$, 12 As Q$(122), 2 As Q$(123), 12 As Q$(124), 2 As Q$(125), 2 As Q$(126), 2 As Q$(127), 2 As Q$(128), 12 As Q$(129), 2 As Q$(130), 2 As Q$(131), 2 As Q$(132), 2 As Q$(133), 2 As Q$(134), 200 As Q$, 150 As Q$

    Field #1, 200 As Q$(0), 200 As Q$(199), 184 As Q$(200), 12 As Q$(135), 2 As Q$(136), 12 As Q$(137), 2 As Q$(138), 12 As Q$(139), 2 As Q$(140), 12 As Q$(141), 2 As Q$(142), 12 As Q$(143), 2 As Q$(144), 200 As Q$(201), 80 As Q$(202)

    Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 154 As Q$(200), 12 As Q$(145), 2 As Q$(146), 12 As Q$(147), 2 As Q$(148), 12 As Q$(149), 2 As Q$(150), 12 As Q$(151), 2 As Q$(152), 12 As Q$(153), 2 As Q$(154), 200 As Q$(201), 10 As Q$(202)

    Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 224 As Q$(200), 12 As Q$(155), 2 As Q$(156), 12 As Q$(157), 2 As Q$(158), 12 As Q$(159), 2 As Q$(160), 12 As Q$(161), 2 As Q$(162), 12 As Q$(163), 2 As Q$(164), 140 As Q$(201)

    Field #1, 200 As Q$(0), 200 As Q$, 200 As Q$(199), 194 As Q$(200), 12 As Q$(165), 2 As Q$(166), 12 As Q$(167), 2 As Q$(168), 12 As Q$(169), 2 As Q$(170), 12 As Q$(171), 2 As Q$(172), 12 As Q$(173), 2 As Q$(174), 70 As Q$(201)

    Field #1, 232 As Q$(0), 200 As Q$, 200 As Q$(199), 232 As Q$(200), 12 As Q$(175), 2 As Q$(176), 12 As Q$(177), 2 As Q$(178), 12 As Q$(179), 2 As Q$(180), 12 As Q$(181), 2 As Q$(182), 12 As Q$(183), 2 As Q$(184)

    Get #1, teamIdx

    targetTeam$ = RTrim$(Q$(1))
    teamNick$ = RTrim$(Q$(2))

    For I = 0 To 7:
        leagRat_DRAFT(I) = CVI(Q$(I + 3))
    Next I

    For I = 0 To 4:
        tmRat_DRAFT(I) = CVI(Q$(I + 11))
    Next I

    For I = 0 To 9
        rbNam_DRAFT$(I) = Q$(I * 5 + 16)
        rbAtt_DRAFT(I) = CVI(Q$(I * 5 + 17))
        rbAvg_DRAFT(I) = CVI(Q$(I * 5 + 18))
        rbRec_DRAFT(I) = CVI(Q$(I * 5 + 19))
        rbRecAvg_DRAFT(I) = CVI(Q$(I * 5 + 20))
    Next I

    For I = 0 To 5
        wrNam_DRAFT$(I) = Q$(I * 3 + 66)
        wrRec_DRAFT(I) = CVI(Q$(I * 3 + 67))
        wrAvg_DRAFT(I) = CVI(Q$(I * 3 + 68))
    Next I

    For I = 0 To 3
        qbNam_DRAFT$(I) = Q$(I * 5 + 84)
        qbPA_DRAFT(I) = CVI(Q$(I * 5 + 85))
        qbPCPct_DRAFT(I) = CVI(Q$(I * 5 + 86))
        qbIntPct_DRAFT(I) = CVI(Q$(I * 5 + 87))
        qbArmRat_DRAFT(I) = CVI(Q$(I * 5 + 88))
    Next I

    For I = 0 To 2
        krNam_DRAFT$(I) = Q$(I * 3 + 104)
        krNumRet_DRAFT(I) = CVI(Q$(I * 3 + 105))
        krRetAvg_DRAFT(I) = CVI(Q$(I * 3 + 106))
    Next I

    For I = 0 To 2
        prNam_DRAFT$(I) = Q$(I * 3 + 113)
        prRet_DRAFT(I) = CVI(Q$(I * 3 + 114))
        prRetAvg_DRAFT(I) = CVI(Q$(I * 3 + 115))
    Next I

    pntNam_DRAFT$(0) = RTrim$(Q$(122))
    puntAvg_DRAFT(0) = CVI(Q$(123))

    For I = 0 To 1
        pkNam_DRAFT$(I) = RTrim$(Q$(I * 5 + 124))
        FGA_DRAFT(I) = CVI(Q$(I * 5 + 125))
        FGPct_DRAFT(I) = CVI(Q$(I * 5 + 126))
        PAT_DRAFT(I) = CVI(Q$(I * 5 + 127))
        PATPct_DRAFT(I) = CVI(Q$(I * 5 + 128))
    Next I

    For I = 0 To 14
        sackNam_DRAFT$(I) = Q$(I * 2 + 135)
        numSack_DRAFT(I) = CVI(Q$(I * 2 + 136))
    Next I

    For I = 0 To 9
        intNam_DRAFT$(I) = Q$(I * 2 + 165)
        numInt_DRAFT(I) = CVI(Q$(I * 2 + 166))
    Next I

    Close #1

    For I = 0 To 15:
        C2!(I) = 0
        C3!(I) = 0
    Next

    U = 0

    For I = 1 To 9
        If rbAtt_DRAFT(I) = 0 Then
            U = U + 1
        Else
            C3!(I) = rbAtt_DRAFT(I) - rbAtt_DRAFT(I - 1 - U)
            U = 0
        End If
    Next

    For I = 1 To 9:
        rbAtt_DRAFT(I) = C3!(I)
    Next

    For I = 0 To 5:
        rbRec_DRAFT(I + 10) = wrRec_DRAFT(I)
    Next

    U = 0

    For I = 1 To 15
        If rbRec_DRAFT(I) = 0 Then
            U = U + 1
        Else
            C2!(I) = rbRec_DRAFT(I) - rbRec_DRAFT(I - 1 - U)
            U = 0
        End If
    Next

    For I = 1 To 15:
        rbRec_DRAFT(I) = C2!(I)
    Next

    For I = 0 To 5:
        wrRec_DRAFT(I) = rbRec_DRAFT(I + 10)
    Next

    For I = 3 To 1 Step -1
        If qbPA_DRAFT(I) <> 0 Then
            qbPA_DRAFT(I) = qbPA_DRAFT(I) - qbPA_DRAFT(I - 1)
        End If
    Next

    For I = 2 To 1 Step -1
        If krNumRet_DRAFT(I) <> 0 Then
            krNumRet_DRAFT(I) = krNumRet_DRAFT(I) - krNumRet_DRAFT(I - 1)
        End If
    Next

    For I = 2 To 1 Step -1
        If prRet_DRAFT(I) <> 0 Then
            prRet_DRAFT(I) = prRet_DRAFT(I) - prRet_DRAFT(I - 1)
        End If
    Next

    For I = 1 To 1 Step -1
        If FGPct_DRAFT(I) <> 0 Then
            FGPct_DRAFT(I) = FGPct_DRAFT(I) - FGPct_DRAFT(I - 1)
        End If
    Next

    For I = 1 To 1 Step -1
        If PATPct_DRAFT(I) <> 0 Then
            PATPct_DRAFT(I) = PATPct_DRAFT(I) - PATPct_DRAFT(I - 1)
        End If
    Next

End Sub

