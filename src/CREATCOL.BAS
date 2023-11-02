'----------------------------------------
'          CREATCOL Subroutine
'----------------------------------------
'This subroutine provides a menu for the
'user to choose options related to the
'creation and editing of conferences
Sub CREATCOL ()

    _Title "3-in-1 Football - Manage Conferences"

    Shared statsZ1$()

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
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            For X = 1 To 30
                statsZ1$(X) = "XXX"
            Next

            div1$ = ""
            div2$ = ""

            Select Case Selection%

                Case 1:
                    Call DefineConference(conference$, div1$, div2$)
                    Call ReviewEditConference(conference$, div1$, div2$)

                Case 2:
                    conference$ = ChooseFile$ (diskPaths$(2) + "*.LIB", "CONFERENCE", FileExit%)

                    If FileExit% <> 27 And FileExit% <> 0 Then
                        Call ReviewEditConference(conference$, div1$, div2$)
                    End If

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'      DefineConference Subroutine
'----------------------------------------
'This subroutine assists the user with the
'input of paramters related to the conf-
'erence's general naming and setup
Sub DefineConference (conference$, div1$, div2$)

    Do

        Color 15, 0
        Cls

		Call MenuBox (6, 8, 61, 7, 15, 1)

        Do

            Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR CONFERENCE: ": Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS."
            Locate , 12: Print "SAMPLE NAMES ARE SEC, SWC, 94-ACC"
            Locate 7, 37: Input conference$

            If conference$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE? (Y/N) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    'Determine division names
    'Color 15, 0
    'Print
    'Print
    'Locate , 12: Input "ENTER NAME OF DIVISION 1 (ENTER FOR NONE) "; div1$
    'Locate , 12: Input "ENTER NAME OF DIVISION 2 (ENTER FOR NONE) "; div2$

    FG = 0
    BG = 7

    Cls
    Color 15, 1

    Locate 5, 1: QPrint "Name for Division 1 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div1$, 50), FG, BG, -1
    Locate 7, 1: QPrint "Name for Division 2 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div2$, 50), FG, BG, -1

    GetDiv1:
    Locate 5, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div1$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv2:
    Locate 7, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div2$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here

    Done:
    Color 14, 1
    Locate 23, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo GetDiv2
    If X$ = "N" GoTo GetDiv1
    If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

End Sub


'----------------------------------------
'     ReviewEditConference Subroutine
'----------------------------------------
'This subroutine provides an interface to
'the user allowing them to choose / clear
'teams from the conference
Sub ReviewEditConference (conference$, div1$, div2$)

    Shared statsZ1%()

    Shared statsZ1$(), statsZ2$()

    If _FileExists(diskPaths$(2) + conference$ + ".LIB") Then

        Open diskPaths$(2) + conference$ + ".LIB" For Input As #1

        Input #1, LG%

        If LG% <> 0 Then

            Cls
            Print "THE FILE YOU TRIED TO OPEN IS NOT COLLEGE FORMAT"
            Print "EITHER TRY AGAIN OR USE THE PRO OPTION TO EDIT"
            Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"

            I$ = GetKeyPress$

            wrongFormat% = 1

        Else

            Input #1, conference$, div1$, div2$

            For X = 1 To 30:
                Input #1, statsZ1$(X)
                Input #1, statsZ1%(X)
                Input #1, statsZ2$(X)
            Next

        End If

        Close #1

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
                Color 15: Print " "; statsZ1$(I);

                Locate , 50
                Color 2: Print Using "##"; I + 15;
                Color 15: Print " "; statsZ1$(I + 15)

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
                    'Call GetTeam_CREATCOL(statsZ1$(B), statsZ2$(B), statsZ1%(B))
                    Call ChooseTeam(statsZ2$(B), NTMS, statsZ1%(B), statsZ1$(B))
                Else
                    'Remove team
                    statsZ1$(B) = "XXX"
                End If

            End If 'Done checking if ready to save

        Loop Until choice = 3

        Call SaveConferences(conference$, div1$, div2$)

    End If

End Sub


'----------------------------------------
'       SaveConferences Subroutine
'----------------------------------------
'This subroutine writes the conference's
'team memberships and definition to 
'each of the corresponding files (.LIB, .TMS)
Sub SaveConferences (conference$, div1$, div2$)

    Shared statsZ1%()

    Shared statsZ1$(), statsZ2$()

    'Ready to save work
    Open diskPaths$(2) + conference$ + ".LIB" For Output As #1

    Print #1, 0
    Print #1, conference$
    Print #1, div1$
    Print #1, div2$

    For X = 1 To 30:
        Print #1, statsZ1$(X)
        Print #1, statsZ1%(X)
        Print #1, statsZ2$(X)
    Next

    Close #1

    For X = 1 To 30

        If statsZ1$(X) = "XXX" Then
            statsZ1$(X) = Chr$(42)
        Else
            teamName$ = statsZ1$(X)
			statFile$ = nameStatFile$(teamName$)
            statsZ1$(X) = statFile$
        End If

    Next X

    Open diskPaths$(2) + conference$ + ".TMS" For Output As #2

    Write #2, conference$
    Write #2, div1$
    Write #2, div2$

    For X = 1 To 30:
        Write #2, statsZ1$(X)
    Next

    Close #2

End Sub
