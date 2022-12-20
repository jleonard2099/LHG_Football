Sub CREATPRO ()

    Shared Z1$()

    _Title "3-in-1 Football - Create League"

    ReDim menuChoices$(1 To 3)

    FG = 0
    BG = 7

    For X = 1 To 30:
        Z1$(X) = "XXX"
    Next

    Do

        Cls

        Call TITLE
        menuChoices$(1) = "CREATE / DEFINE NEW LEAGUES     "
        menuChoices$(2) = "ADD / REVIEW LEAGUE MEMBERS     "
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
            Select Case Selection%
                Case 1: Call DefineLeaguePro
                Case 2: Call ReviewLeague
                Case 3: Cls
            End Select
        End If

    Loop Until Selection% = 2 Or ExitCode% = 27

End Sub

'----------------------------------------
'       DefineLeaguePro Subroutine
'----------------------------------------
'...explanation...

Sub DefineLeaguePro

    Shared Z1$(), Z2$()
    Shared PA%(), Z1%()

    Do
        Do
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

            Locate 7, 32
            Locate , 12: Print "ENTER NAME FOR LEAGUE: ": Print
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

    Loop Until UCase$(I$) = "N"

    'Determine Division names
    Color 15, 0
    Print: Print
    Locate , 12: Input "ENTER NAME OF DIVISION 1 (ENTER FOR NONE) "; D1$
    Locate , 12: Input "ENTER NAME OF DIVISION 2 (ENTER FOR NONE) "; D2$
    Locate , 12: Input "ENTER NAME OF DIVISION 3 (ENTER FOR NONE) "; D3$

    Call ViewProLeague(LG$, D1$, D2$, D3$)

    'Save the work
    Open diskPaths$(2) + LG$ + ".LIB" For Output As #2

    Print #2, 1
    Print #2, LG$
    Print #2, D1$
    Print #2, D2$
    Print #2, D3$

    For X = 1 To 30:
        Print #2, Z1$(X)
        Print #2, Z1%(X)
        Print #2, Z2$(X)
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
    Write #2, D1$
    Write #2, D2$
    Write #2, D3$

    For X = 1 To 30: Write #2, Z1$(X): Next

    Close 2

End Sub


'----------------------------------------
'       ViewProLeague Subroutine
'----------------------------------------
'...explanation...
Sub ViewProLeague (LG$, D1$, D2$, D3$)

    Shared Z1$(), Z2$()
    Shared PA%(), Z1%()

    Do
        Cls
        Print
        Color 14: Print "  "; LG$; " LEAGUE"
        Print
        Color 15
        Print Tab(6); D1$; Tab(30); D2$; Tab(55); D3$: Print

        For I = 1 To 10
            Print Using "  ##"; I;: Print " "; Z1$(I);
            Locate , 25
            Print Using "  ##"; I + 10;: Print " "; Z1$(I + 10);
            Locate , 50
            Print Using "  ##"; I + 20;: Print " "; Z1$(I + 20)
        Next I

        Locate 20, 3

        Color 2
        Print "1-ADD TEAM"
        Print "  2-REMOVE TEAM"
        Print "  3-END AND SAVE"

        Do
            I$ = GetKeyPress$
            choice = Val(I$)
        Loop Until choice >= 1 And choice <= 3

        If I$ <> "3" Then

            Locate 20, 30: Print "TEAM # (1-30) ";

            N = 1

            Do
                Input B
            Loop Until B >= 1 And B <= 30

            If I$ = "1" Then

                Do

                    validTeam% = 1

                    Color 15, 0
                    Cls

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

                    If NY$ = "" Then

                        Call SelectTeam_CREATPRO(NY$, NTMS)

                    Else
                        YN$ = NY$
                        Erase teamNames$

                        NTMS = 0

                        Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934

                        Y = LOF(1)

                        Field #1, 15 As Q$(0), 640 As X$

                        For I1 = 1 To Y / 934
                            Get #1, I1
                            For I2 = 1 To 15
                                If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then validTeam% = 0
                            Next I2
                            If validTeam% = 1 Then
                                NTMS = NTMS + 1
                                teamNames$(NTMS) = Q$(0)
                                teamIndex%(NTMS) = I1
                            End If
                        Next I1

                        Close 1

                        TS% = Int((NTMS - 1) / 80)

                        Call SelectTeam_CREATPRO(YN$, NTMS)

                    End If

                    A$ = Q$(1)
                    A$ = RTrim$(A$)

                    If BO% = 1 Then
                        BO% = 0
                    Else

                        Locate 2, 33
						Color PA%(2), PA%(1): Print A$

                        Do
                            Locate , 33: Print "ANY CHANGE (YN)";
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    End If

                Loop Until UCase$(I$) = "N"

                Color 15, 0

                Z1$(B) = A$
                Z2$(B) = YN$
                Z1%(B) = B8%

            Else
                'I$ = 2
                Z1$(B) = "XXX"
            End If

        End If

    Loop Until I$ = "3"

End Sub

'----------------------------------------
'    SelectTeam_CREATPRO Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_CREATPRO (YN$, NTMS)

    Shared PA%()

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

        B8% = teamIndex%(Selection%)

        Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934

        Field #1, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)

        Get #1, B8%

        For I1 = 0 To 4: PA%(I1) = CVI(Q$(I1 + 11)): Next I1

        Close 1

    Else

        BO% = 1

    End If

End Sub

'----------------------------------------
'       ReviewLeague Subroutine
'----------------------------------------
'...explanation...

Sub ReviewLeague ()

    Shared Z1$(), Z2$()
    Shared PA%(), Z1%()

    wrongFormat% = 0

    Do
        'Display list of files...

        Spec$ = diskPaths$(2) + "*.LIB"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%
        If Count% = 0 Then

            Locate 10, 8
            Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            ReDim leagueFiles$(0 To Count%)
            For X = 1 To Count%
                leagueFiles$(X) = Space$(12)
            Next
            leagueFiles$(0) = Spec$
            Call ReadFile(leagueFiles$())

            Do
                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT LEAGUE TO SCHEDULE": Color 15
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
                Print "-SELECTS LEAGUE";

                _MouseShow "DEFAULT"
                MAMenu leagueFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then
					Color 15, 1
                    targetFile$ = leagueFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate , 33: Print "ANY CHANGE (YN)";
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

                If LG% <> 1 Then
                    Close 2
                    Cls
                    Print "THE FILE YOU TRIED TO OPEN IS IN COLLEGE FORMAT"
                    Print "EITHER TRY AGAIN OR USE THE COLLEGE OPTION TO EDIT"
                    Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
                    I$ = GetKeyPress$
                    wrongFormat% = 1
                Else
                    Input #2, LG$
                    Input #2, D1$
                    Input #2, D2$
                    Input #2, D3$

                    For X = 1 To 30:
                        Input #2, Z1$(X)
                        Input #2, Z1%(X)
                        Input #2, Z2$(X)
                    Next

                    Close 2

                End If

            End If 'Done checking for ESC

        End If 'Done checking for file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or wrongFormat% = 0

    Call ViewProLeague(LG$, D1$, D2$, D3$)

End Sub
