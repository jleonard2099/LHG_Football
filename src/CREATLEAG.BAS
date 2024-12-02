'----------------------------------------
'          CREATLEAG Subroutine
'----------------------------------------
'This subroutine provides a menu for the
'user to choose options related to the
'creation and editing of leagues
Sub CREATLEAG ()

    _Title "3-in-1 Football - Manage Leagues"

    Shared memberName$()

    ReDim menuChoices$(1 To 3)

    Do

        Cls

        Call TITLE
        menuChoices$(1) = "CREATE A LEAGUE       "
        menuChoices$(2) = "MANAGE LEAGUE MEMBERS "
        menuChoices$(3) = "RETURN TO LEADERS MENU"
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            For X = 1 To 30:
                memberName$(X) = "XXX"
            Next

            div1$ = ""
            div2$ = ""
            div3$ = ""

            Select Case SELECTION%

                Case 1:
                    Call DefineLeaguePro(league$, div1$, div2$, div3$)
                    leagueFile$ = league$
                    Call ReviewEditLeague(leagueFile$, league$, div1$, div2$, div3$)

                Case 2:
                    Do
                        leagueFile$ = ChooseFile$ (diskPaths$(2) + "*.TMI", "LEAGUE", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then
                            Call ReviewEditLeague(leagueFile$, league$, div1$, div2$, div3$)
                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'       DefineLeaguePro Subroutine
'----------------------------------------
'This subroutine assists the user with the
'input of paramters related to the league's
'general naming and setup
Sub DefineLeaguePro (league$, div1$, div2$, div3$)

    Shared memberName$()

    Do

        Color 15, 0
        Cls

        Call MenuBox (6, 8, 61, 7, 15, 1)

        Do

            Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR LEAGUE: ": Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS."
            Locate , 12: Print "SAMPLE NAMES ARE SEC, SWC, 94-ACC"
            Locate 7, 37: Input league$

            If league$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE? (Y/N) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

    'Determine Division names
    'Color 15, 0
    'Print
    'Print
    'Locate , 12: Input "ENTER NAME OF DIVISION 1 (ENTER FOR NONE) "; div1$
    'Locate , 12: Input "ENTER NAME OF DIVISION 2 (ENTER FOR NONE) "; div2$
    'Locate , 12: Input "ENTER NAME OF DIVISION 3 (ENTER FOR NONE) "; div3$

    FG = 0
    BG = 7

    Cls
    Color 15, 1

    Locate 5, 1: QPrint "Name for Division 1 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div1$, 50), FG, BG, -1
    Locate 7, 1: QPrint "Name for Division 2 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div2$, 50), FG, BG, -1
    Locate 9, 1: QPrint "Name for Division 3 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div3$, 50), FG, BG, -1

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
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv3:
    Locate 9, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div3$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here

    Done:
    Color 14, 1
    Locate 23, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo GetDiv3
    If X$ = "N" GoTo GetDiv1
    If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow


End Sub


'----------------------------------------
'      ReviewEditLeague Subroutine
'----------------------------------------
'This subroutine provides an interface to
'the user allowing them to choose / clear
'teams from the league definition.
Sub ReviewEditLeague (leagueFile$, league$, div1$, div2$, div3$)

    Shared memberIdx()

    Shared memberName$(), memberYear$()

    If _FileExists(diskPaths$(2) + leagueFile$ + ".TMI") Then

        Open diskPaths$(2) + leagueFile$ + ".TMI" For Input As #1

        Input #1, statType

        If statType <> 1 Then

            Cls
            Print "THE FILE YOU TRIED TO OPEN IS IN COLLEGE FORMAT"
            Print "EITHER TRY AGAIN OR USE THE COLLEGE OPTION TO EDIT"
            Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"

            I$ = GetKeyPress$

            wrongFormat% = 1

        Else

            Input #1, league$, div1$, div2$, div3$

            For X = 1 To 30:
                Input #1, memberName$(X)
                Input #1, memberIdx(X)
                Input #1, memberYear$(X)
            Next

        End If

        Close #1

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
                Color 15: Print " "; Left$(memberName$(I), 20);

                Locate , 25
                Color 2: Print Using "  ##"; I + 10;
                Color 15: Print " "; Left$(memberName$(I + 10), 20);

                Locate , 50
                Color 2: Print Using "  ##"; I + 20;
                Color 15: Print " "; Left$(memberName$(I + 20), 20)

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
                    teamNbr = 0
                    Call ChooseTeam(memberYear$(B), NTMS, memberIdx(B), memberName$(B))
                Else
                    'Remove team
                    memberName$(B) = "XXX"
                End If

            End If 'Done checking if ready to save

        Loop Until choice = 3

        Call SaveLeague(leagueFile$, league$, div1$, div2$, div3$)

    End If

End Sub


'----------------------------------------
'        SaveLeague Subroutine
'----------------------------------------
'This subroutine writes the league team
'memberships and definition to each of
'the corresponding files (.TMI, .TMS)
Sub SaveLeague (leagueFile$, league$, div1$, div2$, div3$)

    Shared memberIdx()

    Shared memberName$(), memberYear$()

    Open diskPaths$(2) + leagueFile$ + ".TMI" For Output As #1

    Print #1, 1

    Print #1, league$
    Print #1, div1$
    Print #1, div2$
    Print #1, div3$

    For X = 1 To 30:
        Print #1, memberName$(X)
        Print #1, memberIdx(X)
        Print #1, memberYear$(X)
    Next

    Close #1

    Erase memberName$
    
    For X = 1 To 30

        If memberName$(X) = "XXX" Then
            memberName$(X) = Chr$(42)
        Else
            teamName$ = memberName$(X)
            memberName$(X) = nameStatFile$(teamName$)
        End If

    Next X

    Open diskPaths$(2) + leagueFile$ + ".TMS" For Output As #2

    Write #2, league$
    Write #2, div1$
    Write #2, div2$
    Write #2, div3$

    For X = 1 To 30:
        Write #2, memberName$(X)
    Next

    Close #2

End Sub
