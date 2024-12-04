'----------------------------------------
'        ReadLeagueOld Subroutine
'----------------------------------------
Sub ReadLeagueOld (leagueFile$, league$, div1$, div2$, div3$, wrongFormat%)

    Shared memberIdx()

    Shared memberName$(), memberYear$()

    For X = 1 To TEAMS_PER_LEAGUE:
        memberName$(X) = "XXX"
        memberIdx(X) = -1
        memberYear$(X) = "XXX"
    Next

    div1$ = ""
    div2$ = ""
    div3$ = ""

    If _FileExists(leagueFile$) Then

        Open leagueFile$ For Input As #1

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

End Sub


'----------------------------------------
'        SaveLeagueOld Subroutine
'----------------------------------------
Sub SaveLeagueOld (leagueFile$, league$, div1$, div2$, div3$)

    Shared memberIdx()

    Shared memberName$(), memberYear$()

    Dim statFileNames$(30)

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
            statFileNames$(X) = Chr$(42)
        Else
            teamName$ = memberName$(X)
            statFileNames$(X) = nameStatFile$(teamName$)
        End If

    Next X

    Open diskPaths$(2) + leagueFile$ + ".TMS" For Output As #2

    Write #2, league$
    Write #2, div1$
    Write #2, div2$
    Write #2, div3$

    For X = 1 To 30:
        Write #2, statFileNames$(X)
    Next

    Close #2

End Sub


'----------------------------------------
'        ReadLeagueNew Subroutine
'----------------------------------------
Sub ReadLeagueNew (leagueFile$, league$, div1$, div2$, div3$, div4$, wrongFormat%)

    Shared memberIdx()

    Shared memberName$(), memberYear$()

    For X = 1 To TEAMS_PER_LEAGUE:
        memberName$(X) = "XXX"
        memberIdx(X) = -1
        memberYear$(X) = "XXX"
    Next

    div1$ = ""
    div2$ = ""
    div3$ = ""
    div4$ = ""

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

            Input #1, league$, div1$, div2$, div3$, div4$

            For X = 1 To 30:
                Input #1, memberName$(X)
                Input #1, memberIdx(X)
                Input #1, memberYear$(X)
            Next

        End If

        Close #1

    End If

End Sub


'----------------------------------------
'     SaveLeagueConverted Subroutine
'----------------------------------------
'This routine saves the modified data back
'to the original team file
Sub SaveLeagueConverted (leagueFile$, league$, div1$, div2$, div3$, div4$)

    Shared memberIdx()

    Shared memberName$(), memberYear$()

    Dim statFileNames$(TEAMS_PER_LEAGUE)

    leagStatFile$ = Left$(leagueFile$, Len(leagueFile$) - 8) + ".TMS"

    Open leagueFile$ For Output As #1

    Print #1, 1

    Print #1, league$
    Print #1, div1$
    Print #1, div2$
    Print #1, div3$
    Print #1, div4$

    For X = 1 To TEAMS_PER_LEAGUE:
        Print #1, memberName$(X)
        Print #1, memberIdx(X)
        Print #1, memberYear$(X)
    Next

    Close #1

    For X = 1 To TEAMS_PER_LEAGUE

        If memberName$(X) = "XXX" Then
            statFileNames$(X) = Chr$(42)
        Else
            teamName$ = memberName$(X)
            statFileNames$(X) = nameStatFile$(teamName$)
        End If

    Next X

    Kill leagStatFile$

    Open leagStatFile$ For Output As #2

    Write #2, league$
    Write #2, div1$
    Write #2, div2$
    Write #2, div3$
    Write #2, div4$

    For X = 1 To TEAMS_PER_LEAGUE:
        Write #2, statFileNames$(X)
    Next

    Close #2

End Sub


'----------------------------------------
'      ConvertLeague4to5 Subroutine
'----------------------------------------
' Reads in existing team data, one team
' at-a-time. For each team, it tries
' to calculate what actual stats it can.
' From there, the updated data is saved.
Sub ConvertLeague4to5 (targetFile$, silent)

    newFile$ = targetFile$ + ".NEW"

    If targetFile$ <> "" Then

        Call ReadLeagueOld(targetFile$, league$, div1$, div2$, div3$, notPro)

        If notPro = 1 Then

            MsgText$ = "ERROR!! THIS IS NOT A PRO LEAGUE. PRESS ANY KEY TO RETURN TO MAIN MENU"
            BoxWidth = 20
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Call SaveLeagueConverted(newFile$, league$, div1$, div2$, div3$, "")

            Kill targetFile$

            Call FCopy(newFile$, targetFile$, Buff$, copyErr%)

            Kill newFile$

            If silent = 0 Then
                result& = _MessageBox("Success!", "The schedule file has been converted for use", "ok", "info", 1)
            End If

        End If

    End If

End Sub


'----------------------------------------
'      ConvertLeague5to4 Subroutine
'----------------------------------------
Sub ConvertLeague5to4 (targetFile$, silent)

    oldFile$ = targetFile$ + ".OLD"

    If targetFile$ <> "" Then

        Call ReadLeagueNew(targetFile$, league$, div1$, div2$, div3$, div4$, notPro)

        If notPro = 1 Then

            MsgText$ = "ERROR!! THIS IS NOT A PRO LEAGUE. PRESS ANY KEY TO RETURN TO MAIN MENU"
            BoxWidth = 20
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Call SaveLeagueConverted(targetFile$, league$, div1$, div2$, div3$, "")

            'Is this all accurate ???
            Kill targetFile$

            Call FCopy(oldFile$, targetFile$, Buff$, copyErr%)

            Kill oldFile$

            If silent = 0 Then
                result& = _MessageBox("Success!", "The schedule file has been converted for use", "ok", "info", 1)
            End If

        End If

    End If

End Sub


'----------------------------------------
'       ConvertAllLeague Subroutine
'----------------------------------------
' Quick and dirty script to convert all
' team files in the "Schedule" folder
Sub ConvertAllLeague ()

    fileSpec$ = diskPaths$(2) + "*.TMI"

    Count% = FileCount%(fileSpec$)
    ReDim foundFiles$(0 To Count%)
    foundFiles$(0) = fileSpec$
    Call ReadFile(foundFiles$())

    For X = 1 To Count%
        target$ = diskPaths$(2) + foundFiles$(X)
        Print "Converting "; target$
        Call ConvertLeague4to5(target$, 1)
    Next X

End Sub
