Sub CREATCOL ()

    Shared Z1$()

    _Title "3-in-1 Football - Create Conference"

    ReDim menuChoices$(1 To 4)

    FG = 0
    BG = 7

    Do

        Cls

        Call TITLE
        menuChoices$(1) = "CREATE / DEFINE NEW CONFERENCES "
        menuChoices$(2) = "ADD / REVIEW CONFERENCE MEMBERS "
        menuChoices$(3) = "RETURN TO STANDINGS/LEADERS MENU"
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 11, 57, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            For X = 1 To 30
                Z1$(X) = "XXX"
            Next

            Select Case Selection%
                Case 1:
                    Call DefineConference
                Case 2:
                    Call ReviewConf_Coll
                Case 3:
                    Cls

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       DefineConference Subroutine
'----------------------------------------
'...explanation...
Sub DefineConference ()

    Color 15, 0

    Cls

    JJ = 7
    Locate 6, 8: Color 15, 1
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

    Do

        Do
            Locate 7, 32
            Locate , 12: Print "ENTER NAME FOR CONFERENCE: ": Print
            Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
            Locate , 12: Print "SPACES."
            Locate , 12: Print "SAMPLE NAMES ARE SEC, SWC, 94-ACC"
            Locate 7, 37: Input LG$

        Loop Until LG$ <> ""

        LG$ = Left$(LG$, 8)

        Do
            Locate 13, 12: Print "ANY CHANGE (YN) "
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

    'Determine division names
    Color 15, 0
    Print
    Print
    Locate , 12: Input "ENTER NAME OF DIVISION 1 (ENTER FOR NONE) "; div1$
    Locate , 12: Input "ENTER NAME OF DIVISION 2 (ENTER FOR NONE) "; div2$

    Call ViewConference_Coll(LG$, div1$, div2$)
    Call SaveConferences(LG$, div1$, div2$)

End Sub


'----------------------------------------
'     SaveConferences Subroutine
'----------------------------------------
'...explanation...
Sub SaveConferences (LG$, div1$, div2$)

    Shared Z1$(), Z2$()
    Shared statsPA%(), statsZ1%()

    'Ready to save work
    Open diskPaths$(2) + LG$ + ".LIB" For Output As #2

    Print #2, 0
    Print #2, LG$
    Print #2, div1$
    Print #2, div2$

    For X = 1 To 30:
        Print #2, Z1$(X): Print #2, statsZ1%(X): Print #2, Z2$(X)
    Next

    Close 2

    For X = 1 To 30

        If Z1$(X) = "XXX" Then
            Z1$(X) = Chr$(42)
        Else

            For XX = 4 To 11:
                If Mid$(Z1$(X), XX, 1) = " " Or Mid$(Z1$(X), XX, 1) = "/" Or Mid$(Z1$(X), XX, 1) = "." Then Mid$(Z1$(X), XX, 1) = "_"
            Next XX

            For XX = 12 To 14:
                If Mid$(Z1$(X), XX, 2) = "ST" Then Mid$(Z1$(X), 9, 1) = "_": Mid$(Z1$(X), 10, 2) = "ST"
            Next XX

            QQ = 8
            For XX = 11 To 4 Step -1:
                If Mid$(Z1$(X), XX, 1) = "_" Then
                    QQ = XX - 4
                Else
                    Exit For
                End If
            Next XX

            YR$ = Left$(Z1$(X), 2)
            Z1$(X) = Mid$(Z1$(X), 4, QQ) + "." + YR$

        End If

    Next X

    J$ = Left$(LG$, 8)
    Q$ = J$ + ".TMS"

    Open diskPaths$(2) + Q$ For Output As #2
    Write #2, LG$
    Write #2, div1$
    Write #2, div2$
    For X = 1 To 30: Write #2, Z1$(X): Next
    Close 2

End Sub


'----------------------------------------
'     ViewConference_Coll Subroutine
'----------------------------------------
'...explanation...
Sub ViewConference_Coll (LG$, div1$, div2$)

    Shared Z1$(), Z2$()
    Shared statsPA%(), statsZ1%()

    Do

        Cls
        Print
        Color 14: Print "  "; LG$; " CONFERENCE"

        Print
        Color 15
        Locate , 6: Print div1$
        Locate , 53: Print div2$

        For I = 1 To 15
            Print Using "  ##"; I;
            Print " "; Z1$(I);
            Locate , 50
            Print Using "##"; I + 15;
            Print " "; Z1$(I + 15)
        Next I

        Locate 21, 3

        Color 2
        Print "1-ADD TEAM"
        Print "  2-REMOVE TEAM"
        Print "  3-END AND SAVE"

        Do
            I$ = GetKeyPress$
            choice = Val(I$)
        Loop Until choice >= 1 And choice <= 3

        If I$ <> "3" Then

            Locate 20, 30
            Print "TEAM # (1-30) ";

            N = 1

            Do
                Input B
            Loop Until B >= 1 And B <= 30

            If I$ = "1" Then

                Do

                    validTeam% = 1

                    181 '
                    Color 15, 0: Cls
                    JJ = 7
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
                    Locate , 12: Print
                    Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
                    Locate 7, 27: Input NY$

                    If NY$ = "" And YN$ <> "" Then
                        Call SelectTeam_CREATCOL(YN$, NTMS, teamNbr)
                    Else
                        YN$ = RTrim$(NY$)
                        Erase teamNames$
                        NTMS = 0

                        Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934

                        Y = LOF(1)

                        Field #1, 15 As Q$(0), 640 As X$

                        For I1 = 1 To Y / 934
                            Get #1, I1
                            For I2 = 1 To 3
                                If Asc(Left$(Q$(0), I2)) <= 32 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 2)) = "UN" Then validTeam% = 0: Exit For
                            Next I2

                            If validTeam% = 1 Then
                                NTMS = NTMS + 1
                                teamNames$(NTMS) = Q$(0)
                                teamIndex%(NTMS) = I1
                            End If

                        Next I1

                        Close 1

                        Call SelectTeam_CREATCOL(YN$, NTMS, teamNbr)

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

                    End If

                Loop Until UCase$(I$) = "N"

                Color 15, 0
                Z1$(B) = A$
                Z2$(B) = YN$
                statsZ1%(B) = teamNbr

            Else
                'Delete a team
                Z1$(B) = "XXX"

            End If 'Done acting

        End If 'Done checking for 3

    Loop Until I$ = "3"

    Call SaveConferences(LG$, div1$, div2$)

End Sub


'----------------------------------------
'       ReviewConf_Coll Subroutine
'----------------------------------------
'...explanation...
Sub ReviewConf_Coll ()

    Shared Z1$(), Z2$()
    Shared statsZ1%()

    wrongFormat% = 0

    Do
        'Display list of files...

        Spec$ = diskPaths$(2) + "*.LIB"
		Count% = FCount%(Spec$)

        If Count% = 0 Then

            Locate 10, 8
            Call CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            ReDim conferenceFiles$(0 To Count)

            For X = 1 To Count
                conferenceFiles$(X) = Space$(12)
            Next

            conferenceFiles$(0) = Spec$

            Call ReadFile(conferenceFiles$())

            Do
                Cls
                Color , 1

                Cls
                Locate , 28

                Color 14
                Print "SELECT CONFERENCE TO SCHEDULE": Color 15
                Selection% = 1
                Start% = 1
                FileFGColr% = 15: FileBGColr% = 1
                HiLiteFGColr% = 0: HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";: Color 15
                Print "-SELECTS CONFERENCE";

                _MouseShow "DEFAULT"
                MAMenu conferenceFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = conferenceFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                targetFile$ = RTrim$(targetFile$)
                LG$ = Left$(targetFile$, Len(targetFile$) - 4)

                Open diskPaths$(2) + LG$ + ".LIB" For Input As #2

                Input #2, LG%

                If LG% <> 0 Then
                    Close 2
                    Cls
                    Print "THE FILE YOU TRIED TO OPEN IS NOT COLLEGE FORMAT"
                    Print "EITHER TRY AGAIN OR USE THE PRO OPTION TO EDIT"
                    Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
                    wrongFormat% = 1
                    I$ = GetKeyPress$
                Else

                    Input #2, LG$
                    Input #2, div1$
                    Input #2, div2$

                    For X = 1 To 30:
                        Input #2, Z1$(X)
                        Input #2, statsZ1%(X)
                        Input #2, Z2$(X)
                    Next

                    Close 2
                End If

            End If 'Done checking for ESC

        End If 'Done checking for counts

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or wrongFormat% = 0

    Call ViewConference_Coll(LG$, div1$, div2$)


End Sub

'----------------------------------------
'     SelectTeam_CREATCOL Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_CREATCOL (YN$, NTMS, teamNbr)

    Shared statsPA%()

    Selection% = 1
    Count% = NTMS
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

        teamNbr = teamIndex%(Selection%)
        teamYear$ = RTrim$(YN$)

        Open diskPaths$(0) + "FOOT." + teamYear$ For Random As #1 Len = 934

        Field #1, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)

        Get #1, teamNbr

        For I1 = 0 To 4
            statsPA%(I1) = CVI(Q$(I1 + 11))
        Next I1

        Close 1

    Else

        BO% = 1

    End If

End Sub
