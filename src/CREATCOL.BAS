Sub CREATCOL ()

    _Title "3-in-1 Football - Manage Conferences"

    Shared Z1$()

    ReDim menuChoices$(1 To 4)

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

            div1$ = ""
            div2$ = ""

            Select Case Selection%

                Case 1:
                    Call DefineConference(conference$, div1$, div2$)
                    Call ReviewEditConf(conference$, div1$, div2$)

                Case 2:
                    conference$ = ChooseFile$ (diskPaths$(2) + "*.LIB", "CONFERENCE", ExitCode%)
                    Call ReviewEditConf(conference$, div1$, div2$)

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'      DefineConference Subroutine
'----------------------------------------
'...explanation...
Sub DefineConference (conference$, div1$, div2$)

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

            Locate , 12: Print "ENTER NAME FOR CONFERENCE: ": Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS."
            Locate , 12: Print "SAMPLE NAMES ARE SEC, SWC, 94-ACC"
            Locate 7, 37: Input conference$

            If conference$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE (YN) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    'Determine division names
    Color 15, 0
    Print
    Print
    Locate , 12: Input "ENTER NAME OF DIVISION 1 (ENTER FOR NONE) "; div1$
    Locate , 12: Input "ENTER NAME OF DIVISION 2 (ENTER FOR NONE) "; div2$

End Sub


'----------------------------------------
'     ReviewEditConf Subroutine
'----------------------------------------
'...explanation...
Sub ReviewEditConf (conference$, div1$, div2$)

    Shared statsZ1%()

    Shared Z1$(), Z2$()

    If _FileExists(diskPaths$(2) + conference$ + ".LIB") Then

        Open diskPaths$(2) + conference$ + ".LIB" For Input As #2

        Input #2, LG%

        If LG% <> 0 Then

            Cls
            Print "THE FILE YOU TRIED TO OPEN IS NOT COLLEGE FORMAT"
            Print "EITHER TRY AGAIN OR USE THE PRO OPTION TO EDIT"
            Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"

            I$ = GetKeyPress$

            wrongFormat% = 1

        Else

            Input #2, conference$, div1$, div2$

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
            Print "  "; conference$; " CONFERENCE"
            Print

            Color 12
            Locate , 6: Print div1$;
            Locate , 53: Print div2$

            For I = 1 To 15

                Color 2: Print Using "  ##"; I;
                Color 15: Print " "; Z1$(I);

                Locate , 50
                Color 2: Print Using "##"; I + 15;
                Color 15: Print " "; Z1$(I + 15)

            Next I

            Do

                Locate 21, 1

                Color 14: Print "(1) ";
                Color 15: Print "ADD A TEAM"
                Color 14: Print "(2) ";
                Color 15: Print "REMOVE A TEAM"
                Color 14: Print "(3) ";
                Color 15: Print "END AND SAVE CONFERENCE"

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
                    'Call GetTeam_CREATCOL(Z1$(B), Z2$(B), statsZ1%(B))
                    Call ChooseTeam(Z2$(B), NTMS, statsZ1%(B), Z1$(B))
                Else
                    'Remove team
                    Z1$(B) = "XXX"
                End If

            End If 'Done checking if ready to save

        Loop Until choice = 3

        Call SaveConferences(conference$, div1$, div2$)

    End If

End Sub


'----------------------------------------
'     SaveConferences Subroutine
'----------------------------------------
'...explanation...
Sub SaveConferences (conference$, div1$, div2$)

    Shared statsZ1%()

    Shared Z1$(), Z2$()

    'Ready to save work
    Open diskPaths$(2) + conference$ + ".LIB" For Output As #2

    Print #2, 0
    Print #2, conference$
    Print #2, div1$
    Print #2, div2$

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

    Open diskPaths$(2) + conference$ + ".TMS" For Output As #2

    Write #2, conference$
    Write #2, div1$
    Write #2, div2$

    For X = 1 To 30:
        Write #2, Z1$(X)
    Next

    Close 2

End Sub
