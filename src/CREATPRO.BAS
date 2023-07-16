Sub CREATPRO ()

    _Title "3-in-1 Football - Manage Leagues"

    Shared Z1$()

    ReDim menuChoices$(1 To 3)

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

            For X = 1 To 30:
                Z1$(X) = "XXX"
            Next

            Select Case Selection%

                Case 1:
                    Call DefineLeaguePro(LG$, div1$, div2$, div3$)
                    Call ReviewEditLeague(LG$, div1$, div2$, div3$)

                Case 2:
                    Call ChooseLeague(LG$)
                    Call ReviewEditLeague(LG$, div1$, div2$, div3$)

            End Select

        End If

    Loop Until Selection% = 2 Or ExitCode% = 27

End Sub


'----------------------------------------
'       DefineLeaguePro Subroutine
'----------------------------------------
'...explanation...
Sub DefineLeaguePro (LG$, div1$, div2$, div3$)

    Shared Z1$()

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

        Do

            Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR LEAGUE: ": Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPACES."
            Locate , 12: Print "SAMPLE NAMES ARE SEC, SWC, 94-ACC"
            Locate 7, 37: Input LG$

            If LG$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE (YN) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    'Determine Division names
    Color 15, 0
    Print
    Print
    Locate , 12: Input "ENTER NAME OF DIVISION 1 (ENTER FOR NONE) "; div1$
    Locate , 12: Input "ENTER NAME OF DIVISION 2 (ENTER FOR NONE) "; div2$
    Locate , 12: Input "ENTER NAME OF DIVISION 3 (ENTER FOR NONE) "; div3$

End Sub


'----------------------------------------
'       ChooseLeague Subroutine
'----------------------------------------
'...explanation...
Sub ChooseLeague (league$)

    wrongFormat% = 0

    Do
        'Display list of files...
        Spec$ = diskPaths$(2) + "*.LIB"
        Count% = FCount%(Spec$)

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
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                targetFile$ = RTrim$(targetFile$)
                league$ = Left$(targetFile$, Len(targetFile$) - 4)

            End If 'Done checking for ESC

        End If 'Done checking for file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or wrongFormat% = 0

End Sub


'----------------------------------------
'      ReviewEditLeague Subroutine
'----------------------------------------
'...explanation...
Sub ReviewEditLeague (league$, div1$, div2$, div3$)

    Shared statsZ1%()

    Shared Z1$(), Z2$()

    If _FileExists(diskPaths$(2) + league$ + ".LIB") Then

        Open diskPaths$(2) + league$ + ".LIB" For Input As #2

        Input #2, LG%

        If LG% <> 1 Then

            Cls
            Print "THE FILE YOU TRIED TO OPEN IS IN COLLEGE FORMAT"
            Print "EITHER TRY AGAIN OR USE THE COLLEGE OPTION TO EDIT"
            Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"

            I$ = GetKeyPress$

            wrongFormat% = 1

        Else

            Input #2, league$, div1$, div2$, div3$

            For X = 1 To 30:
                Input #2, Z1$(X)
                Input #2, statsZ1%(X)
                Input #2, Z2$(X)
            Next

        End If

        Close 2

    End If

    If (wrongFormat% <> 1) Then

        Do

            Color 14, 0
            Cls

            Print
            Print "  "; league$; " LEAGUE"
            Print

            Color 12
            Print Tab(6); div1$; Tab(30); div2$; Tab(55); div3$: Print

            For I = 1 To 10

                Color 2: Print Using "  ##"; I;
                Color 15: Print " "; Z1$(I);

                Locate , 25
                Color 2: Print Using "  ##"; I + 10;
                Color 15: Print " "; Z1$(I + 10);

                Locate , 50
                Color 2: Print Using "  ##"; I + 20;
                Color 15: Print " "; Z1$(I + 20)

            Next I

            Do

                Locate 21, 1

                Color 14: Print "(1) ";
                Color 15: Print "ADD A TEAM"
                Color 14: Print "(2) ";
                Color 15: Print "REMOVE A TEAM"
                Color 14: Print "(3) ";
                Color 15: Print "END AND SAVE LEAGUE"

                I$ = GetKeyPress$
                choice = Val(I$)
            Loop Until choice >= 1 And choice <= 3

            If choice <> 3 Then

                Do
                    Locate 22, 35
                    Print "TEAM # (1-30) ";
                    Input B
                Loop Until B >= 1 And B <= 30

                If choice = 1 Then
                    'Add team
                    Call GetTeam_CREATPRO(Z1$(B), Z2$(B), statsZ1%(B))
                Else
                    'Remove team
                    Z1$(B) = "XXX"
                End If

            End If 'Done checking if ready to save

        Loop Until choice = 3

        Call SaveLeague(league$, div1$, div2$, div3$)

    End If

End Sub


'----------------------------------------
'        SaveLeague Subroutine
'----------------------------------------
'...explanation...
Sub SaveLeague (LG$, div1$, div2$, div3$)

    Shared statsZ1%()

    Shared Z1$(), Z2$()

    Open diskPaths$(2) + LG$ + ".LIB" For Output As #2

    Print #2, 1

    Print #2, LG$
    Print #2, div1$
    Print #2, div2$
    Print #2, div3$

    For X = 1 To 30:
        Print #2, Z1$(X)
        Print #2, statsZ1%(X)
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

    Open diskPaths$(2) + LG$ + ".TMS" For Output As #2

    Write #2, LG$
    Write #2, div1$
    Write #2, div2$
    Write #2, div3$

    For X = 1 To 30:
        Write #2, Z1$(X)
    Next

    Close 2

End Sub


'----------------------------------------
'    GetTeam_CREATPRO Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_CREATPRO (A$, YN$, teamNbr)

    Shared teamRatings%()

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

        If NY$ = "" And YN$ <> "" Then
            Call SelectTeam_CREATPRO(YN$, NTMS, teamNbr)
        Else
            YN$ = RTrim$(NY$)
            Erase teamNames$
            NTMS = 0

            Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934

            fileLength& = LOF(1)

            Field #1, 15 As Q$(0), 640 As X$

            For I1 = 1 To fileLength& / 934
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

            Call SelectTeam_CREATPRO(YN$, NTMS, teamNbr)

        End If

        A$ = RTrim$(Q$(1))

        If BO% = 1 Then
            BO% = 0
        Else

            Locate 2, 33
            Color teamRatings%(2), teamRatings%(1): Print A$

            Do
                Color 15, 4
                Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'    SelectTeam_CREATPRO Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_CREATPRO (YN$, NTMS, teamNbr)

    Shared teamRatings%()

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

        For I1 = 0 To 4: teamRatings%(I1) = CVI(Q$(I1 + 11)): Next I1

        Close 1

    Else

        BO% = 1

    End If

End Sub

