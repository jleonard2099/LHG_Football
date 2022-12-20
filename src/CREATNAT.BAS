Sub CREATNAT ()

    Shared Z1$(), Z2$()
    Shared Z1%()

    _Title "3-in-1 Football - Create Organization"

    ReDim menuChoices$(1 To 3)

    'determine if the monitor is mono or color
    FG = 0
    BG = 7

    For X = 1 To 40
        Z1$(X) = "XXX"
        Z2$(X) = "XXX"
        Z1%(X) = -1
    Next

    Do
        Cls

        Call TITLE
        menuChoices$(1) = "ADD / DELETE /REVIEW CONFERENCES"
        menuChoices$(2) = "CREATE / DEFINE LEAGUE & MEMBERS"
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
        I1 = Selection%

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1:
                    Call DefineLeagueNatl(LG$)
                    Call NatlLeagueMenu(LG$)
                Case 2:
                    Call ReviewConf_Natl(LG$)
                    Call NatlLeagueMenu(LG$)
                Case 3:
                    Cls

            End Select
        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       DefineLeagueNatl Subroutine
'----------------------------------------
'...explanation...
Sub DefineLeagueNatl (organization$)

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
            Locate , 12: Print "SAMPLE NAMES ARE NCAA-90, SOUTH, MIDWEST"
            Locate 7, 37: Input organization$

        Loop Until organization$ <> ""

        '-- We'll want to remove this in the future
        '-- since we're not in DOS anymore
        organization$ = Left$(organization$, 8)

        Do
            Locate 13, 12: Print "ANY CHANGE (YN) "
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

End Sub

'----------------------------------------
'       NatlLeagueMenu Subroutine
'----------------------------------------
'...explanation...
Sub NatlLeagueMenu (organization$)

    Shared Z1$()

    Do
        Color 14, 0
        Cls
        Print
        Print "  "; organization$
        Print
        Color 12

        For I = 1 To 10
            Color 2: Print Using "## "; I;: Color 15: Print Z1$(I);
            Locate , 21: Color 2: Print Using "## "; I + 10;: Color 15: Print Z1$(I + 10);
            Locate , 41: Color 2: Print Using "## "; I + 20;: Color 15: Print Z1$(I + 20);
            Locate , 61: Color 2: Print Using "## "; I + 30;: Color 15: Print Z1$(I + 30)
        Next I

        Locate 19, 1
        Color 14: Print "(1) ";: Color 15: Print "ADD A CONFERENCE"
        Color 14: Print "(2) ";: Color 15: Print "REMOVE A CONFERENCE"
        Color 14: Print "(3) ";: Color 15: Print "END AND SAVE LEAGUE"

        Do
            I$ = GetKeyPress$
            choice = Val(I$)
        Loop Until choice >= 1 And choice <= 3

        If choice <> 3 Then
            'Determine Slot
            Locate 20, 30: Print "SLOT # (1-40) ";

            N = 1
            Do
                Input B
            Loop Until B >= 1 And B <= 40

            If I$ = "1" Then
                'Add conference
                Call AddConference(F$)
                Z1$(B) = F$

            Else
                ' Remove conference
                'I$ = "2"
                Z1$(B) = "XXX"
            End If

        End If

    Loop Until I$ = "3"

    Cls

    Open diskPaths$(2) + organization$ + ".CON" For Output As #2

    Print #2, organization$
    For X = 1 To 40
        Print #2, Z1$(X)
    Next
    Close 2

End Sub

'----------------------------------------
'     AddConference Subroutine
'----------------------------------------
'...explanation...
Sub AddConference (targetFile$)
    Do
        'Display list of files...

        Spec$ = diskPaths$(2) + "*.LIB"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else

            ReDim conferenceFiles$(0 To Count%)

            For X = 1 To Count%
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
                Print "SELECT CONFERENCE": Color 15
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

                If ExitCode% <> 27 Then 'Exit Sub
					Color 15, 1
                    targetFile$ = conferenceFiles$(Selection%)
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
                targetFile$ = Left$(targetFile$, Len(targetFile$) - 4)
            End If

        End If

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or targetFile$ <> ""

End Sub

'----------------------------------------
'     ReviewConf_Natl Subroutine
'----------------------------------------
'...explanation...
Sub ReviewConf_Natl (targetFile$)

    Shared Z1$()

    Do
        'Display list of files...

        Spec$ = diskPaths$(2) + "*.CON"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then

            Locate 10, 8
            Call CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            ReDim conferenceFiles$(0 To Count%)

            For X = 1 To Count%
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
                        Locate , 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

					Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                targetFile$ = RTrim$(targetFile$)
                targetFile$ = Left$(targetFile$, Len(targetFile$) - 4)

                Open diskPaths$(2) + targetFile$ + ".CON" For Input As #2
                Input #2, LG$

                For X = 1 To 40
                    Input #2, Z1$(X)
                Next

                Close 2
                Color 15, 1

            End If

        End If

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or targetFile$ <> ""

End Sub
