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

            div1$ = ""
            div2$ = ""
            div3$ = ""

            Select Case Selection%

                Case 1:
                    Call DefineLeaguePro(league$, div1$, div2$, div3$)
                    Call ReviewEditLeague(league$, div1$, div2$, div3$)

                Case 2:
                    league$ = ChooseFile$ (diskPaths$(2) + "*.LIB", "LEAGUE", ExitCode%)
                    Call ReviewEditLeague(league$, div1$, div2$, div3$)

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'       DefineLeaguePro Subroutine
'----------------------------------------
'...explanation...
Sub DefineLeaguePro (leagueName$, div1$, div2$, div3$)

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
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS."
            Locate , 12: Print "SAMPLE NAMES ARE SEC, SWC, 94-ACC"
            Locate 7, 37: Input leagueName$

            If leagueName$ <> "" Then

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
                    'Call GetTeam_CREATPRO(Z1$(B), Z2$(B), statsZ1%(B))
                    Call ChooseTeam(Z2$(B), NTMS, statsZ1%(B), Z1$(B))
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
Sub SaveLeague (leagueName$, div1$, div2$, div3$)

    Shared statsZ1%()

    Shared Z1$(), Z2$()

    Open diskPaths$(2) + leagueName$ + ".LIB" For Output As #2

    Print #2, 1

    Print #2, leagueName$
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
            teamName$ = Z1$(X)
			statFile$ = nameStatFile$(teamName$)
            Z1$(X) = statFile$
        End If

    Next X

    Open diskPaths$(2) + leagueName$ + ".TMS" For Output As #2

    Write #2, leagueName$
    Write #2, div1$
    Write #2, div2$
    Write #2, div3$

    For X = 1 To 30:
        Write #2, Z1$(X)
    Next

    Close 2

End Sub


